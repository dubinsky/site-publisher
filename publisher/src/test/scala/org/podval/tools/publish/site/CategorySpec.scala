package org.podval.tools.publish.site

import org.podval.tools.publish.util.{Files, SiteOptions}
import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import java.nio.file.{Files as NioFiles, Path as NioPath}

final class CategorySpec extends AnyFunSuite:
  private def withSite(files: Map[String, String])(body: (Site, File) => Unit): Unit =
    val path: NioPath = NioFiles.createTempDirectory("site-publisher-categories")
    try
      val dir: File = path.toFile
      files.foreach: (relative, content) =>
        Files.write(File(dir, relative), content)
      val target: File = File(dir, "_site")
      val site: Site = Site(SiteOptions(
        sourceDirectoryPath = dir.getAbsolutePath,
        targetDirectoryNameOpt = Some(target.getAbsolutePath),
        treatErrorsAsWarnings = false,
        logLevelOpt = Some("WARN")
      ))
      site.generate()
      body(site, target)
    finally
      NioFiles.walk(path).sorted(java.util.Comparator.reverseOrder()).forEach(NioFiles.delete(_))

  private val config: String =
    """title: Category Fixture
      |description: category tests
      |url: http://categories.test
      |author: Test
      |email: test@categories.test
      |graph:
      |  enabled: true
      |""".stripMargin

  private def note(categories: String, body: String): String =
    s"""---
       |$categories
       |---
       |$body
       |""".stripMargin

  private def html(target: File, relative: String): String =
    val file: File = File(target, relative)
    assert(file.isFile, s"missing $relative under $target")
    Files.read(file)

  private def between(page: String, startMarker: String, endMarker: String): String =
    val start: Int = page.indexOf(startMarker)
    assert(start >= 0, s"missing $startMarker in $page")
    val end: Int = page.indexOf(endMarker, start + startMarker.length)
    assert(end >= 0, s"missing $endMarker after $startMarker")
    page.substring(start, end)

  test("category value keeps the wiki target and drops a display alias") {
    assert(Categories.linkTarget("[[Books]]").contains("Books"))
    assert(Categories.linkTarget("[[Books|library]]").contains("Books"))
    assert(Categories.linkTarget("  Books  ").contains("Books"))
    assert(Categories.linkTarget("[[Books#Favorites]]").contains("Books#Favorites"))
    assert(Categories.linkTarget("").isEmpty)
    assert(Categories.linkTarget("[[]]").isEmpty)
  }

  test("membership lists authored hubs, chips, graph links, and drops base embeds") {
    withSite(Map(
      "_site_config.yml" -> config,
      "index.md" -> "Home.\n",
      "Books.md" -> note(
        """categories:
          |  - "[[Books]]"
          |""".stripMargin,
        """Shelf notes.
          |
          |![[Books.base#Favorites]]
          |
          |Still here.
          |""".stripMargin
      ),
      "Movies.md" -> "Films.\n",
      "Amy.md" -> note(
        """tags:
          |  - to-read
          |categories:
          |  - "[[Books]]"
          |  - "[[Books]]"
          |""".stripMargin,
        "Amy on books.\n"
      ),
      "Zed.md" -> note(
        """categories:
          |  - Books
          |""".stripMargin,
        "Zed on books.\n"
      ),
      "Alias.md" -> note(
        """categories:
          |  - "[[Books|library]]"
          |""".stripMargin,
        "Alias on books.\n"
      ),
      "Fragment.md" -> note(
        """categories:
          |  - "[[Books#nope]]"
          |""".stripMargin,
        "Fragment on books.\n"
      ),
      "Both.md" -> note(
        """categories:
          |  - "[[Books]]"
          |  - "[[Movies]]"
          |""".stripMargin,
        "See [[Movies]].\n"
      ),
      "Mention.md" -> "See [[Books]].\n",
      "Missing.md" -> note(
        """categories:
          |  - "[[No Such]]"
          |""".stripMargin,
        "Missing hub.\n"
      ),
      "References/Blade.md" -> note(
        """categories:
          |  - "[[Movies]]"
          |""".stripMargin,
        "Blade.\n"
      )
    )): (_, target) =>
      val books: String = html(target, "Books.html")
      val members: String = between(books, "category-members", "u-url")
      assert(books.contains("Shelf notes."), books)
      assert(books.contains("Still here."), books)
      assert(!books.contains("Books.base"), books)
      assert(!books.contains("Favorites"), books)
      assert(members.contains("Alias"), members)
      assert(members.contains("Amy"), members)
      assert(members.contains("Both"), members)
      assert(members.contains("Fragment"), members)
      assert(members.contains("Zed"), members)
      assert(!members.contains("Mention"), members)
      assert(!members.contains("Blade"), members)
      assert(!members.contains("href=\"/Books.html\""), members)
      assert(members.indexOf("Alias") < members.indexOf("Amy"), members)
      assert(members.indexOf("Amy") < members.indexOf("Both"), members)
      assert(members.indexOf("Both") < members.indexOf("Fragment"), members)
      assert(members.indexOf("Fragment") < members.indexOf("Zed"), members)
      assert(members.split("page-ref").length - 1 == 5, members)

      val movies: String = between(html(target, "Movies.html"), "category-members", "u-url")
      assert(movies.contains("Blade"), movies)
      assert(movies.contains("Both"), movies)
      assert(!movies.contains("Amy"), movies)
      assert(movies.indexOf("Blade") < movies.indexOf("Both"), movies)

      val amy: String = html(target, "Amy.html")
      val amyHeader: String = between(amy, "post-meta", "post-content")
      assert(amyHeader.split("page-category").length - 1 == 1, amyHeader)
      assert(amyHeader.contains("href=\"/Books.html\""), amyHeader)
      assert(amyHeader.contains("page-tag"), amyHeader)
      assert(amyHeader.contains("to-read"), amyHeader)

      val alias: String = between(html(target, "Alias.html"), "post-meta", "post-content")
      assert(alias.contains("href=\"/Books.html\""), alias)
      assert(!alias.contains("library"), alias)

      val both: String = between(html(target, "Both.html"), "post-header", "post-content")
      assert(both.indexOf("href=\"/Books.html\"") < both.indexOf("href=\"/Movies.html\""), both)

      assert(books.contains("class=\"backlinks\""), books)
      assert(between(books, "class=\"backlinks\"", "</main>").contains("Mention"), books)

      assert(!html(target, "index.html").contains("category-members"))
      assert(!File(target, "No Such.html").exists)

      val errors: String = html(target, "errors.html")
      assert(errors.contains("unresolved category '[[No Such]]'"), errors)
      assert(!errors.contains("Books.base"), errors)
      assert(!errors.contains("unresolved transclusion"), errors)

      val graph: String = html(target, "graph.json")
      val bothMovies: String = """"source":"/Both.html","target":"/Movies.html","kind":"link""""
      assert(graph.split(java.util.regex.Pattern.quote(bothMovies), -1).length - 1 == 1, graph)
      assert(graph.contains(""""source":"/Amy.html","target":"/Books.html","kind":"link""""), graph)
      assert(graph.contains(""""source":"/Mention.html","target":"/Books.html","kind":"link""""), graph)
      assert(!graph.contains(""""source":"/Books.html","target":"/Books.html""""), graph)
  }
