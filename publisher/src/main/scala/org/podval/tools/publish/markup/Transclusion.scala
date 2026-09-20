package org.podval.tools.publish.markup

import org.podval.tools.publish.page.{AuthoredContent, ChunkedMarkupPage, FullMarkupPage, NamedWindows, Page,
  PageContent}
import org.podval.tools.publish.site.PageError
import org.podval.tools.publish.util.Strings
import org.podval.xml.{CssClass, Xml, XmlAttribute, XmlElement}

enum Region derives CanEqual:
  case Whole
  case Preamble
  case Section(id: String)
  case Block(id: String)

  /** Strict (irreflexive) containment on `on`. */
  def contains(other: Region, on: PageContent): Boolean =
    this match
      case Region.Whole =>
        other match
          case Region.Whole => false
          case _ => true
      case Region.Preamble =>
        other match
          case Region.Block(id) => on.ids.sectionOf(id).isEmpty
          case _ => false
      case Region.Section(id) =>
        val nested: Seq[String] = on.toc.flatten.find(_.id == id).toSeq.flatMap(_.flatten).map(_.id)
        other match
          case Region.Section(otherId) => nested.contains(otherId)
          case Region.Block(blockId) =>
            on.ids.sectionOf(blockId).exists(sid => sid == id || nested.contains(sid))
          case _ => false
      case Region.Block(_) => false

object Region:
  val maxHops: Int = 32

  private def isDocSection(element: Xml.Element): Boolean =
    element.isElement(XmlElement.Div) && element.hasClass("section")

  def wouldReenter(
    stack: List[(Page, Region)],
    page: Page,
    target: Region,
    content: PageContent
  ): Boolean =
    stack.exists: (openPage, open) =>
      openPage == page && (target == open || target.contains(open, content))

  def ofLinkFragment(
    content: PageContent,
    fragment: Option[Link.ToFragment],
    requested: Boolean
  ): Option[Region] =
    fragment match
      case None => Option.when(!requested)(Whole)
      case Some(section: Link.ToSection) => Some(Section(section.id))
      case Some(block: Link.ToBlock) => Some(Block(block.id))
      case Some(toId: Link.ToId) =>
        content.xml.gather(el => Option.when(el.getId.contains(toId.id))(el)).headOption.map: el =>
          if isDocSection(el) then Section(toId.id) else Block(toId.id)

  def hostOf(anchor: Xml.Element, content: PageContent): Region =
    val id: Option[String] = anchor.getId.filter(_.nonEmpty)
    val found: Seq[Region] = content.xml.gatherWithContext(
      gatherElement = (element, context) =>
        Option.when(id.exists(element.getId.contains)):
          context match
            case Some(ctx) if WikiBlock.is(ctx) =>
              Block(ctx.getId.getOrElse(element.getId.getOrElse("")))
            case Some(ctx) if isDocSection(ctx) =>
              Section(ctx.getId.getOrElse(element.getId.getOrElse("")))
            case _ =>
              Preamble
      ,
      isContext = el => isDocSection(el) || WikiBlock.is(el)
    )
    found.headOption.getOrElse(Preamble)

  def resolveFragment(content: PageContent, fragment: String): Option[Link.ToFragment] =
    val trimmed: String = fragment.trim
    if trimmed.startsWith("^")
    then content.blocks.resolve(id = trimmed.drop(1).trim)
    else if trimmed.contains("#")
    then content.toc.resolveSection(names = trimmed.split('#').map(_.trim).toSeq)
    else content.ids.resolve(trimmed).orElse(content.toc.resolveSection(names = Seq(trimmed)))

object Transclusion:
  object ChromeClass extends CssClass("transclusion")
  object HeaderClass extends CssClass("transclusion-header")
  object SourceClass extends CssClass("transclusion-source")
  object ContentClass extends CssClass("transclusion-content")
  object UnresolvedClass extends CssClass("unresolved-transclusion")
  object LoopClass extends CssClass("transclusion-loop")

  def isChrome(element: Xml.Element): Boolean = element.has(ChromeClass)

  enum Source derives CanEqual:
    case Ok(page: FullMarkupPage, region: Region, content: PageContent)
    case MissingPage
    case UnresolvedFragment
    case NonAuthored

  def authoredSource(link: Link, href: String): Source =
    val requested: Option[String] = Strings.splitFirst(href, '#')._2.map(_.trim).filter(_.nonEmpty)
    val page0: Page = link.page.real
    val owner: Option[(FullMarkupPage, Region)] = page0 match
      case chunk: ChunkedMarkupPage =>
        val default: Region = chunk.sectionId.map(Region.Section(_)).getOrElse(Region.Whole)
        Some((chunk.markupPage, default))
      case full: FullMarkupPage =>
        full.doc match
          case Some(_: AuthoredContent) => Some((full, Region.Whole))
          case _ => None
      case _ =>
        None
    owner match
      case None => Source.NonAuthored
      case Some((page, default)) =>
        page.content match
          case None => Source.NonAuthored
          case Some(content) =>
            requested match
              case None => Source.Ok(page, default, content)
              case Some(fragment) =>
                Region.resolveFragment(content, fragment).flatMap: toFragment =>
                  Region.ofLinkFragment(content, Some(toFragment), requested = true)
                match
                  case None => Source.UnresolvedFragment
                  case Some(region) => Source.Ok(page, region, content)

  def expand(
    selected: Xml.Element,
    host: PageContent,
    sectionId: Option[String],
    isTerminal: Boolean
  ): (Xml.Element, Map[String, Footnote]) =
    val isChunked: Boolean = sectionId.isDefined || !isTerminal
    val fromPage: FullMarkupPage = host.source.page.asFullMarkupPage.get
    val initial: List[(Page, Region)] = sectionId.toList.map(id => (fromPage, Region.Section(id)))
    val counter: Counter = Counter()
    val footnotes: FootnoteSink = FootnoteSink()
    val xml: Xml.Element = expandElement(
      selected,
      fromPage = fromPage,
      fromContent = host,
      stack = initial,
      hops = 0,
      counter = counter,
      reportErrors = !isChunked,
      host = host,
      footnotes = footnotes
    )
    (xml, footnotes.toMap)

  private final class Counter:
    private var n: Int = 0
    def next(): Int =
      n += 1
      n

  private final class FootnoteSink:
    private val buf: scala.collection.mutable.ListBuffer[(String, Footnote)] =
      scala.collection.mutable.ListBuffer.empty
    def add(id: String, footnote: Footnote): Unit = buf += (id -> footnote)
    def toMap: Map[String, Footnote] = buf.toMap

  private def expandElement(
    element: Xml.Element,
    fromPage: FullMarkupPage,
    fromContent: PageContent,
    stack: List[(Page, Region)],
    hops: Int,
    counter: Counter,
    reportErrors: Boolean,
    host: PageContent,
    footnotes: FootnoteSink
  ): Xml.Element =
    val expanded: Xml.Nodes = expandNodes(
      Seq(element),
      fromPage,
      fromContent,
      stack,
      hops,
      counter,
      reportErrors,
      host,
      footnotes
    )
    expanded.flatMap(_.asElement).headOption.getOrElse(element)

  private def expandNodes(
    nodes: Xml.Nodes,
    fromPage: FullMarkupPage,
    fromContent: PageContent,
    stack: List[(Page, Region)],
    hops: Int,
    counter: Counter,
    reportErrors: Boolean,
    host: PageContent,
    footnotes: FootnoteSink
  ): Xml.Nodes =
    nodes.flatMap: node =>
      node.asElement match
        case None => Seq(node)
        case Some(element) if element.isNamed(XmlElement.Code.localName) => Seq(element)
        case Some(element) if isChrome(element) => Seq(element)
        case Some(element) if WikiLink.isTranscluded(element) =>
          Seq(expandHop(
            element,
            fromPage,
            fromContent,
            stack,
            hops,
            counter,
            reportErrors,
            host,
            footnotes
          ))
        case Some(element) if element.isElement(XmlElement.P) =>
          val kids: Xml.Nodes = expandNodes(
            element.getChildren,
            fromPage,
            fromContent,
            stack,
            hops,
            counter,
            reportErrors,
            host,
            footnotes
          )
          splitParagraph(element.setChildren(kids))
        case Some(element) =>
          Seq(element.setChildren(expandNodes(
            element.getChildren,
            fromPage,
            fromContent,
            stack,
            hops,
            counter,
            reportErrors,
            host,
            footnotes
          )))

  private def splitParagraph(paragraph: Xml.Element): Xml.Nodes =
    val kids: Xml.Nodes = paragraph.getChildren
    val asides: Seq[Xml.Element] = kids.flatMap(_.asElement).filter(isChrome)
    if asides.isEmpty then Seq(paragraph)
    else if asides.size == 1 && kids.forall(node => node.isWhitespace || node.asElement.exists(isChrome))
    then asides
    else
      def loop(rest: Xml.Nodes): Xml.Nodes =
        if rest.isEmpty then Seq.empty
        else
          val (run: Xml.Nodes, after: Xml.Nodes) = rest.span(node => !node.asElement.exists(isChrome))
          val runPart: Xml.Nodes =
            if run.forall(_.isWhitespace) then Seq.empty else Seq(paragraph.setChildren(run))
          after.headOption.flatMap(_.asElement) match
            case Some(aside) if isChrome(aside) => runPart ++ Seq(aside) ++ loop(after.tail)
            case _ => runPart
      loop(kids)

  private def expandHop(
    anchor: Xml.Element,
    fromPage: FullMarkupPage,
    fromContent: PageContent,
    stack: List[(Page, Region)],
    hops: Int,
    counter: Counter,
    reportErrors: Boolean,
    host: PageContent,
    footnotes: FootnoteSink
  ): Xml.Element =
    val hostRegion: Region = Region.hostOf(anchor, fromContent)
    val href: String = anchor.getHref.getOrElse("")
    val kind: Option[LinkKind] = LinkKind.of(anchor)
    val withHost: List[(Page, Region)] = stack :+ (fromPage, hostRegion)
    fromPage.site.pages.resolve(href, kind, fromPage) match
      case None =>
        freeze(
          anchor,
          PageError.UnresolvedTransclusion,
          s"unresolved transclusion '$href'",
          reportErrors,
          host
        )
      case Some(link) =>
        authoredSource(link, href) match
          case Source.MissingPage | Source.NonAuthored =>
            freeze(
              anchor,
              PageError.UnresolvedTransclusion,
              s"transclusion target is not authored content '$href'",
              reportErrors,
              host
            )
          case Source.UnresolvedFragment =>
            freeze(
              anchor,
              PageError.UnresolvedTransclusion,
              s"unresolved transclusion fragment '$href'",
              reportErrors,
              host
            )
          case Source.Ok(toPage, target, toContent) =>
            if hops >= Region.maxHops then
              freeze(
                anchor,
                PageError.TransclusionLoop,
                loopMessage(hops, withHost :+ (toPage, target)),
                reportErrors,
                host,
                loop = true
              )
            else if Region.wouldReenter(withHost, toPage, target, toContent) then
              freeze(
                anchor,
                PageError.TransclusionLoop,
                loopMessage(hops, withHost :+ (toPage, target)),
                reportErrors,
                host,
                loop = true
              )
            else
              fromPage.site.transclusions.cached((toPage, target)):
                regionXml(toContent, target, toPage)
              match
                case None =>
                  freeze(
                    anchor,
                    PageError.UnresolvedTransclusion,
                    s"unresolved transclusion fragment '$href'",
                    reportErrors,
                    host
                  )
                case Some(raw) =>
                  val preNested: Xml.Element = stripForCopy(raw)
                  val nestedStack: List[(Page, Region)] = withHost :+ (toPage, target)
                  val expanded: Xml.Element = preNested.setChildren(expandNodes(
                    preNested.getChildren,
                    fromPage = toPage,
                    fromContent = toContent,
                    stack = nestedStack,
                    hops = hops + 1,
                    counter = counter,
                    reportErrors = reportErrors,
                    host = host,
                    footnotes = footnotes
                  ))
                  val n: Int = counter.next()
                  val prefix: String = s"transclusion-$n-"
                  copyFootnoteClosure(expanded, toContent, prefix, footnotes)
                  val rewritten: Xml.Element = prefixDirect(expanded, n, toPage, toContent)
                  wrap(rewritten, toPage, target, toContent, anchor, href)

  private def copyFootnoteClosure(
    expanded: Xml.Element,
    toContent: PageContent,
    prefix: String,
    footnotes: FootnoteSink
  ): Unit =
    def loop(pending: Seq[String], seen: Set[String]): Unit =
      pending.headOption match
        case None => ()
        case Some(id) if seen.contains(id) => loop(pending.drop(1), seen)
        case Some(id) =>
          val inner: Seq[String] = toContent.footnotes.get(id) match
            case None => Seq.empty
            case Some(footnote) =>
              val prefixedNodes: Xml.Nodes = footnote.nodes.map: node =>
                node.asElement.fold(node)(Footnote.prefixCorrelationTree(_, prefix))
              footnotes.add(
                prefix + id,
                Footnote.remapped(
                  footnote,
                  prefix + id,
                  footnote.number,
                  footnote.scope,
                  prefixedNodes
                )
              )
              Footnote.linkIds(footnote.nodes)
          loop(pending.drop(1) ++ inner, seen + id)
    loop(Footnote.linkIds(expanded), Set.empty)

  private def loopMessage(hops: Int, stack: List[(Page, Region)]): String =
    val frames: String = stack.map((page, region) => s"${page.publishedPath}:$region").mkString(" -> ")
    s"transclusion loop after $hops hops: $frames"

  private def freeze(
    anchor: Xml.Element,
    kind: PageError.Kind,
    message: String,
    reportErrors: Boolean,
    host: PageContent,
    loop: Boolean = false
  ): Xml.Element =
    if reportErrors then host.source.error(kind, message)
    var result: Xml.Element = anchor.add(UnresolvedClass)
    if loop then result = result.add(LoopClass)
    result.setClasses(result.getClasses.filterNot(_ == Link.InternalLinkClass.name))

  private def regionXml(
    content: PageContent,
    region: Region,
    sourcePage: Page
  ): Option[Xml.Element] =
    region match
      case Region.Whole =>
        content.doc match
          case authored: AuthoredContent =>
            Some(authored.selectedXml(
              content.toc.select(content.xml, None, isTerminal = true),
              sourcePage
            ))
          case _ => None
      case Region.Section(id) =>
        content.xml.gather(el => Option.when(el.isElement(XmlElement.Div) && el.hasClass("section") && el.getId.contains(id))(el)).headOption
      case Region.Block(id) =>
        content.xml.gather(el => Option.when(el.getId.contains(id))(el)).headOption
      case Region.Preamble =>
        None

  private def stripForCopy(xml: Xml.Element): Xml.Element =
    xml.transform: element =>
      element.setChildren(element.getChildren.filterNot: node =>
        node.asElement.exists: child =>
          child.has(Toc.PlaceholderClass) || Citation.isPlaceholder(child)
      )

  private def wrap(
    copy: Xml.Element,
    sourcePage: FullMarkupPage,
    region: Region,
    sourceContent: PageContent,
    anchor: Xml.Element,
    href: String
  ): Xml.Element =
    val permalink: String = chromePermalink(sourcePage, region)
    val label: String = chromeLabel(sourcePage, region, sourceContent, anchor, href)
    val regionName: String = region match
      case Region.Whole => "whole"
      case Region.Section(_) => "section"
      case Region.Block(_) => "block"
      case Region.Preamble => "preamble"
    val sourceLink: Xml.Element = Xml
      .element(XmlElement.A)
      .add(Link.InternalLinkClass)
      .add(SourceClass)
      .setHref(permalink)
      .setText(label)
    val header: Xml.Element = Xml
      .element("header")
      .add(HeaderClass)
      .setChildren(Seq(NamedWindows.setXmlTarget(sourceLink, sourcePage)))
    val bodyNodes: Xml.Nodes = region match
      case Region.Whole => copy.getChildren
      case _ => Seq(copy)
    val body: Xml.Element = Xml
      .element(XmlElement.Div)
      .add(ContentClass)
      .setChildren(bodyNodes)
    Xml
      .element("aside")
      .add(ChromeClass)
      .set("data-source", permalink)
      .set("data-region", regionName)
      .setChildren(Seq(header, body))

  private def chromePermalink(sourcePage: FullMarkupPage, region: Region): String =
    val path: String = sourcePage.publishedPath.toString
    region match
      case Region.Whole | Region.Preamble => path
      case Region.Section(id) => s"$path#$id"
      case Region.Block(id) => s"$path#$id"

  private def chromeLabel(
    sourcePage: FullMarkupPage,
    region: Region,
    sourceContent: PageContent,
    anchor: Xml.Element,
    href: String
  ): String =
    val pageTitle: String = sourcePage.title
    val regionTitle: String = region match
      case Region.Whole | Region.Preamble => pageTitle
      case Region.Section(id) =>
        val heading: String = sourceContent.toc.flatten.find(_.id == id).map(_.title).getOrElse(id)
        s"$pageTitle § $heading"
      case Region.Block(id) => s"$pageTitle § ^$id"
    val defaultText: String = WikiLink.wikiLinkText(transclude = true, href)
    val inner: String = anchor.getText.trim.stripPrefix(WikiLink.startTransclusion).stripSuffix(WikiLink.end).trim
    if inner.nonEmpty && inner != href && anchor.getText.trim != defaultText then
      region match
        case Region.Whole | Region.Preamble => inner
        case Region.Section(_) | Region.Block(_) =>
          val heading: String = regionTitle.drop(pageTitle.length)
          s"$inner$heading"
    else regionTitle

  private def prefixDirect(
    xml: Xml.Element,
    n: Int,
    sourcePage: FullMarkupPage,
    sourceContent: PageContent
  ): Xml.Element =
    val prefix: String = s"transclusion-$n-"
    val ids: Set[String] = directIds(xml)
    val rewritten: Xml.Element = mapDirect(xml): element =>
      var result: Xml.Element = prefixIds(element, prefix, ids)
      result = Footnote.prefixCorrelation(result, prefix)
      result = rewriteAria(result, prefix, ids)
      result = rewriteHref(result, prefix, ids, sourcePage)
      result = rewriteAsset(result, sourcePage)
      result = rewritePb(result, prefix, ids, sourcePage)
      result
    mapDirect(rewritten): element =>
      attachTips(element, sourcePage, sourceContent)

  private def directIds(xml: Xml.Element): Set[String] =
    def loop(element: Xml.Element): Seq[String] =
      if isChrome(element) then Seq.empty
      else element.getId.toSeq.filter(_.nonEmpty) ++ element.flatMapElements(loop)
    loop(xml).toSet

  private def mapDirect(xml: Xml.Element)(f: Xml.Element => Xml.Element): Xml.Element =
    if isChrome(xml) then xml
    else
      val self: Xml.Element = f(xml)
      self.setChildren(self.getChildren.map: node =>
        node.asElement match
          case Some(child) if isChrome(child) => child
          case Some(child) => mapDirect(child)(f)
          case None => node
      )

  private def prefixIds(element: Xml.Element, prefix: String, ids: Set[String]): Xml.Element =
    var result: Xml.Element = element
    result.getId.filter(ids.contains).foreach: id =>
      result = result.setId(prefix + id)
    result.get("xml:id").filter(ids.contains).foreach: id =>
      result = result.set("xml:id", prefix + id)
    result

  private def rewriteAria(element: Xml.Element, prefix: String, ids: Set[String]): Xml.Element =
    Seq("aria-describedby", "aria-labelledby").foldLeft(element): (el, attr) =>
      el.get(attr).fold(el): value =>
        val rewritten: String = value.split(' ').toSeq.map: token =>
          val trimmed: String = token.trim
          if trimmed.isEmpty then trimmed
          else if ids.contains(trimmed) then prefix + trimmed
          else if trimmed.endsWith("-tip") && ids.contains(trimmed.dropRight(4)) then prefix + trimmed
          else trimmed
        .mkString(" ")
        el.set(attr, rewritten)

  private def rewriteHref(
    element: Xml.Element,
    prefix: String,
    ids: Set[String],
    sourcePage: FullMarkupPage
  ): Xml.Element =
    if !element.isA then element
    else if org.podval.tools.publish.markup.Section.isPermalink(element) then
      element.getHref.filter(_.startsWith("#")).fold(element): hash =>
        val id: String = hash.drop(1)
        element.setHref(s"${sourcePage.publishedPath}#$id")
    else element.getHref match
      case Some(href) if href.startsWith("#") =>
        val id: String = href.drop(1)
        if ids.contains(id) then element.setHref(s"#$prefix$id")
        else element.setHref(s"${sourcePage.publishedPath}#$id")
      case Some(href) if Link.isInternal(element) =>
        val kind: Option[LinkKind] = LinkKind.of(element)
        sourcePage.site.pages.resolve(href, kind, sourcePage) match
          case None => element
          case Some(linkTo) =>
            var result: Xml.Element = element.setHref(linkTo.url)
            if !linkTo.isIntrapage then result = NamedWindows.setXmlTarget(result, linkTo.page)
            result
      case _ =>
        element

  private def rewriteAsset(element: Xml.Element, sourcePage: FullMarkupPage): Xml.Element =
    sourcePage.source.fold(element): source =>
      sourcePage.site.pages.resolveAsset(
        element,
        sourcePage,
        source,
        reportMissing = false
      )

  private def rewritePb(
    element: Xml.Element,
    prefix: String,
    ids: Set[String],
    sourcePage: FullMarkupPage
  ): Xml.Element =
    if !Pb.is(element) then element
    else
      val originalId: Option[String] = element.getId.filter(_.nonEmpty)
      val prefixed: Xml.Element = originalId.filter(ids.contains).fold(element): id =>
        element.setId(prefix + id)
      sourcePage.site.pages.facsimilePage(sourcePage) match
        case None => prefixed
        case Some(viewer) =>
          originalId.fold(prefixed): id =>
            NamedWindows.setXmlTarget(
              prefixed.setHref(s"${viewer.publishedPath}#$id"),
              viewer
            )

  private def attachTips(
    element: Xml.Element,
    sourcePage: FullMarkupPage,
    sourceContent: PageContent
  ): Xml.Element =
    if !Link.isInternal(element) || org.podval.tools.publish.markup.Section.isPermalink(element) || WikiLink.isTranscluded(element)
    then element
    else element.getHref.flatMap: href =>
      val fragment: Option[String] = Strings.splitFirst(href, '#')._2.map(_.trim).filter(_.nonEmpty)
      fragment.flatMap: id =>
        sourceContent.glossaryDefinitions.get(id).map: definition =>
          Glossary.tip.attachTip(element, definition)
        .orElse:
          sourceContent.bibliographyDefinitions.get(id).map: definition =>
            BibliographyItem.tip.attachTip(element.add(Citation.CiteClass), definition)
    .getOrElse(element)
