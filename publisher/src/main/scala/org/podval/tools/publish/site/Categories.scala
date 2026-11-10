package org.podval.tools.publish.site

import org.podval.metadata.Language
import org.podval.tools.publish.page.{AuthoredHarvest, FullMarkupPage, MarkupPage, NamedWindows, Page}
import org.podval.tools.publish.util.Strings
import org.podval.xml.Xml
import org.podval.xml.dsl.{*, given}

final class CategoryLink(
  val from: MarkupPage,
  val to: Page,
  val fromInline: Boolean
)

/** Membership from a page's `categories` property to an authored hub.
  * Not a `LinkKind`: the value is a wiki target, resolved like a body link.
  */
final class Categories:
  private var membershipVar: List[CategoryLink] = Nil

  def membership: Seq[CategoryLink] = membershipVar

  // Called from `Site.load` after `Pages.load` has already run `throwIfErrors`.
  // A missing hub is an unresolved link and must not abort generation.
  def resolve(site: Site): Unit =
    membershipVar = site.pages.pages.flatMap:
      case from: MarkupPage =>
        val own: List[String] =
          AuthoredHarvest.ownFrontMatter(from).toList.flatMap(_.categories)
        val inline: List[String] =
          AuthoredHarvest.inlineFrontMatter(from).toList.flatMap(_.categories)
        val (afterOwn, seen) = own.foldLeft((List.empty[CategoryLink], Set.empty[Page])):
          case ((acc, seen), raw) =>
            Categories.accept(site, from, raw, fromInline = false, acc, seen)
        val (afterInline, _) = inline.foldLeft((afterOwn, seen)):
          case ((acc, seen), raw) =>
            Categories.accept(site, from, raw, fromInline = true, acc, seen)
        afterInline
      case _ =>
        Nil

  def chips(from: MarkupPage): Seq[Xml.Element] =
    membershipVar.filter(link => link.from == from && !link.fromInline).map(link => Categories.chip(link.to))

  def html(page: MarkupPage): Option[Xml.Element] =
    page.asFullMarkupPage.flatMap: full =>
      val members: Seq[MarkupPage] = membershipVar
        .filter(_.to == full)
        .map(_.from)
        .distinct
        .sortBy(member => (member.title, member.publishedPath.toString))
      Option.when(members.nonEmpty):
        given Language.Spec = page.site.languageSpec
        page.withInterfaceLang(div(className := "category-members",
          h3(Messages.text("pages.category")),
          Page.pageList(members)
        ))

object Categories:
  private def chip(page: Page): Xml.Element = a(
    className := "page-category",
    href := page.publishedPath.toString,
    NamedWindows.targetAttr(page).map(name => target := name),
    page.title
  )

  private def accept(
    site: Site,
    from: MarkupPage,
    raw: String,
    fromInline: Boolean,
    acc: List[CategoryLink],
    seen: Set[Page]
  ): (List[CategoryLink], Set[Page]) =
    linkTarget(raw) match
      case None =>
        if raw.trim.nonEmpty then unresolved(site, from, raw)
        (acc, seen)
      case Some(ref) =>
        site.pages.resolve(ref, None, from) match
          case None =>
            unresolved(site, from, raw)
            (acc, seen)
          case Some(link) =>
            val to: Page = link.page.real
            if to == from || seen.contains(to) then (acc, seen)
            else (acc :+ CategoryLink(from, to, fromInline), seen + to)

  private def unresolved(site: Site, from: MarkupPage, raw: String): Unit =
    site.error(
      from.sourcePath.getOrElse(from.path),
      PageError.Unresolved,
      s"unresolved category '$raw'"
    )

  /** Wiki target inside a `categories` value: `[[Note]]`, `[[Note|label]]`, or a bare name. */
  private[site] def linkTarget(raw: String): Option[String] =
    val trimmed: String = raw.trim
    if trimmed.isEmpty then None
    else
      val inner: String =
        if trimmed.startsWith("[[") && trimmed.endsWith("]]")
        then trimmed.substring(2, trimmed.length - 2).trim
        else trimmed
      val ref: String = Strings.split(inner, '|')._1.trim
      Option.when(ref.nonEmpty)(ref)
