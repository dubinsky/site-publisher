package org.podval.tools.publish.site

import org.podval.tools.publish.page.{MarkupPage, Page, SyntheticXmlAsset}
import org.podval.tools.publish.util.{Date, Icon}
import org.podval.xml.{Html, HtmlXmlDialect, Xml, XmlAttribute, XmlEncode}
import zio.blocks.html.*
import java.time.{Instant, LocalTime, ZoneId}
import scala.util.Try

// Note: finished by Grok ;)
object Feed:
  val path: Path = Path("feed").withExtension("xml")

  private val maxEntries: Int = 10

  def icon: Icon = Icon.rss

  def feedFooter: Html.Element = a(
    href := Feed.path.toString,
    icon.html,
    span(className := "rss-feed", "RSS feed")
  )

  // jekyll-feed `{% feed_meta %}`: autodiscovery `<link>` in `<head>`.
  def feedMeta(site: Site): Html.Element = link(
    rel := "alternate",
    `type` := "application/atom+xml",
    titleAttr := site.config.title,
    href := s"${site.uri}$path"
  )

final class Feed(site: Site) extends SyntheticXmlAsset(site, Feed.path):
  override protected def iconDefault: Icon = Feed.icon

  override def xmlContent: Xml.Element =
    val posts: List[Page] = site.posts.posts.take(Feed.maxEntries)
    
    // TODO move from here
    val updated: String =
      val dates: List[String] = posts.flatMap: page =>
        page.dateModified.map(formatDate).toSeq ++
        page.dateModifiedGit.map(formatInstant).toSeq ++
        page.date.map(formatDate).toSeq
      dates.maxOption.getOrElse(formatInstant(Instant.now))

    val prologue: Seq[Xml.Element] = Seq(
      el("id", absoluteUrl(Feed.path)),
      el("title", site.config.title).set("type", "html"),
      el("subtitle", site.config.description),
      el("generator", Seo.generatorName).set("uri", Seo.generatorUrl),
      link(
        href = absoluteUrl(Feed.path),
        rel = "self",
        `type` = "application/atom+xml"
      ),
      link(
        href = s"${site.uri}/",
        rel = "alternate",
        `type` = "text/html"
      ),
      el("updated", updated),
      Xml
        .element("author")
        .setChildren(Seq(
          el("name", site.config.author),
          el("email", site.config.email)
        ))
    )
    
    Xml
      .element("feed")
      .set(XmlAttribute.Xmlns, "http://www.w3.org/2005/Atom")
      .setChildren(
        prologue ++ posts.map(entry)
      )
  
  private def entry(page: Page): Xml.Element =
    val url: String = absoluteUrl(page.publishedPath)
    val published: String = page.date.map(formatDate).getOrElse(formatInstant(Instant.now))
    // TODO move out from here
    val updated: String =
      page.dateModified.map(formatDate)
      .orElse(page.dateModifiedGit.map(formatInstant))
      .getOrElse(published)

    Xml
      .element("entry")
      .setChildren(
        Seq(
          el("title", page.title).set("type", "html"),
          link(
            href = url,
            rel = "alternate",
            `type` = "text/html",
            title = Some(page.title)
          ),
          el("published", published),
          el("updated", updated),
          el("id", absoluteUrl(page.publishedPath.withoutHtml)),
          Xml
            .element("author")
            .setChildren(Seq(el("name", author(page))))
        ) ++
          tags(page).map(tag => Xml.element("category").set("term", tag)) ++
          //<![CDATA[ Years ago, I wrote a piece on the sbt build tool: [[2011-11-08-sbt-why]]. Although I dislike how sbt works (and since I looked at the sbt internals I dislike it even more), my main complaint was not about how, but about the fact that sbt exists at all. ]]>
          page.description.map(text => el("summary", text /* TODO use CData */).set("type", "html")).toSeq ++
          //<content type="html" xml:base="http://dub.podval.org/2025/12/22/mill-why.html">
          //<![CDATA[ <p>Years ago, I wrote a piece on the sbt build tool: <a class="wiki-link" href="/2011/11/08/sbt-why.html">sbt: why?!</a>. Although I dislike <em>how</em> sbt works (and since I <a href="https://github.com/dubinsky/scalajs-gradle?tab=readme-ov-file#testing-1" class="web-link">looked at the sbt internals</a> I dislike it even more), my main complaint was not about <em>how</em>, but about the fact that sbt exists <em>at all</em>.</p> <p>It seemed - and still seems - clear to me that enhancing Scala support in the mainstream build tools (<a href="https://maven.apache.org/" class="web-link">Maven</a> in 2011, <a href="https://gradle.org/" class="web-link">Gradle</a> now, at least for me) is more beneficial than creating Scala-specific build tools for the following reasons:</p> <ul> <li> <em>familiarity:</em> developers are likely already familiar with a mainstream build tool, so it makes Scala adoption easier if it does not require learning a new build tool;</li> <li> <em>flexibility:</em> developers are likely already using a mainstream build tool for the existing code, so it makes Scala adoption easier if it does not require switching all non-Scala-specific aspects of the build to a new build tool - especially since mainstream build tools have all kinds of non-Scala-specific plugins and integrations developed by an extended community, which a boutique Scala-specific build tool, with chances of it being adopted by non-Scala developers and enterprises being slim, is unlikely to be able to match;</li> <li> <em>reliability:</em> mature mainstream build tools are by necessity complex and are developed and supported by large teams; a boutique Scala-specific build tool could not possibly provide equivalent level of support: even if designed, developed and maintained by “10x” engineers, it won’t be able to match hundreds of man-years that it took to mature the mainstream build tools.</li> </ul> <p>Recently, I attended a very engaging and entertaining <a href="https://www.meetup.com/boston-area-scala-enthusiasts/events/311173989" class="web-link">talk</a> “Designing Simpler Scala Build Tools with Object-Oriented Programming” by by Li Haoyi. Here is the blurb:</p> <blockquote> <p>Scala’s build tool SBT has always been a pain point for newcomers to the language, but build tools for other languages like Maven or Gradle often aren’t any better. This talk will explore why build tooling is fundamentally such a difficult domain to work in, and how common concepts from object-oriented programming have the potential to simplify the build tool experience. We will end with a demonstration of the Mill build tool that makes use of these ideas, proving out the idea that Scala build tooling has the potential to be much faster, safer, and easier than it is today.</p> </blockquote> <p>The author demonstrated some easy-to-make but hard-to-find mistakes when using sbt, Maven and Gradle, and suggested that a build tool designed around object-oriented concepts avoids similar issues - with the help of IDE tooling that understands Scala, the language used for configuring Mill. Developers not willing to use Scala to configure the build are provided with an alternative configuration language: YAML (!); what are the benefits of the object-oriented design of the build tool in this scenario is not clear ;)</p> <p>As a software developer, I understand every developer’s aspiration to design a programming language, write a compiler, create an operating system (or at least a shell), make a source control system and build a build tool. As an anarcho-capitalist, I wouldn’t, of course, force any developer to put the “good of the community” above his or her own aspirations even if I could.</p> <p>The point remains, though: it seems that enhancing mainstream build tools with proper Scala support <em>is</em> more beneficial to the Scala community than creating more boutique build tools - even if Mill is great ;)</p> <p>Of course, if Scala had special needs that mainstream build tools <em>could not</em> satisfy, we will be justified in an eternal search for the “better sbt”.</p> <p>Indeed, there <em>are</em> some features required for Scala development which official Gradle currently lacks:</p> <ul> <li>include some sources only when building for a specific Scala version;</li> <li>produce artifacts specific to a Scala version used for the particular build;</li> <li>compile, run, and test <a href="https://www.scala-lang.org/" class="web-link">Scala</a> code using non-JVM Scala back-ends: <a href="https://www.scala-js.org/" class="web-link">Scala.js</a> and <a href="https://scala-native.org/" class="web-link">Scala Native</a>;</li> <li>allow some sources to be shared (“cross-compiled”) between some back-ends.</li> </ul> <p>The thing is, all of the above <em>can</em> indeed be done <em>in Gradle</em>; I know this because I wrote a <a href="https://github.com/dubinsky/scalajs-gradle" class="web-link">Gradle plugin</a> that does all of it ;)</p> <p>Of course, it would be better if the <em>official</em> Gradle plugin did all this, and if Gradle engineers <a href="https://github.com/gradle/gradle/issues/32666" class="web-link">cooperated</a> with adding this basic functionality, but neither of those seems likely: official Gradle Scala plugin suffers from chronic neglect by both the “Gradle people” and the “Scala people” :(</p> <p>Gradle the company seems to have given up on attracting Scala developers with its Scala support, and now <a href="https://scaladays.org/blog/gradle/" class="web-link">caters</a> to the sbt users:</p> <blockquote> <p><a href="https://gradle.com/develocity/solutions/sbt/" class="web-link">Develocity</a> by <a href="https://gradle.com/" class="web-link">Gradle</a> gives sbt users powerful ways to speed up builds, shorten feedback cycles, and gain deep insights into build performance.</p> </blockquote> <p>Ironically, the same conference which Gradle co-sponsored and presented its sbt-based advantages at also included the same Mill talk by Li Haoyi: while Gradle switches to sbt, community is shifting to Mill :)</p> <p>Judging by the number of bug reports and GitHub stars, my Gradle plugin is around two orders of magnitude less popular than Mill… Obviously, the community itself does not agree with my assessment of what is in its best interest ;)</p> <p>Nonetheless, I personally remain convinced that enhancing Scala support in Gradle is worthwhile: I think that only “Path 1” from the three paths described by John DeGoes in his excellent <a href="https://degoes.net/articles/new-scala-build-tool" class="web-link">New Scala Build Tool</a> leads anywhere meaningful.</p> <p>At least I can avoid switching to sbt (or Mill) :)</p> ]]>
          //</content>
          (page match
            case markupPage: MarkupPage => markupPage
              .markupContent
              .map(content => HtmlXmlDialect.render(content).trim)
              .map(html =>
                el("content", html /* TODO use CData */)
                  .set("type", "html")
                  .set("xml:base", url)
              )
            case _ => None    
          ).toSeq
      )
  
  private def author(page: Page): String =
    page.asFullMarkupPage.flatMap(_.author).getOrElse(site.config.author)

  private def tags(page: Page): List[String] =
    page.asFullMarkupPage.map(_.tags).getOrElse(List.empty)

  private def absoluteUrl(path: Path): String = s"${site.uri}$path"

  private def link(
    href: String,
    rel: String,
    `type`: String,
    title: Option[String] = None
  ): Xml.Element =
    Xml
      .element("link")
      .set("href", href)
      .set("rel", rel)
      .set("type", `type`)
      .set("title", title)

  private def el(name: String, text: String): Xml.Element =
    // XmlDialect.Plain does not escape text nodes; Atom content must stay well-formed XML.
    Xml.element(name).setText(XmlEncode.encodeXmlSpecials(text))

  private def zone: ZoneId =
    site.config.timezone.flatMap(tz => Try(ZoneId.of(tz)).toOption).getOrElse(ZoneId.systemDefault)

  private def formatDate(date: Date): String = date match
    case date: Date.OffsetTime => date.value.toString
    case date: Date.LocalTime => date.value.atZone(zone).toOffsetDateTime.toString
    case date: Date.Local => date.value.atTime(LocalTime.MIDNIGHT).atZone(zone).toOffsetDateTime.toString

  private def formatInstant(instant: Instant): String =
    instant.atZone(zone).toOffsetDateTime.toString
