package org.podval.tools.publish.page

object AuthoredHarvest:
  /** Trees whose links and transclusions are harvested. Not generated listing XML. */
  def linkContents(page: Page): Seq[PageContent] = page match
    case markup: MarkupPage =>
      markup.content.toSeq.filter(isAuthored) ++
        markup.inlineListProse.toSeq.map(_.content).filter(isAuthored)
    case _ =>
      Nil

  def isAuthored(content: PageContent): Boolean = content.doc match
    case _: DocumentContent | _: EntityContent | _: MarkupContent => true
    case _: StoreContent | _: EntityListsContent => false

  /** Own front matter for chips, graph node `tags`, `Seo.author`, and `Feed` author/tags.
    * A `FullMarkupPage` that still owns the file, or a non-full page whose source is authored.
    * Not `inlineListProse`. Not a `takeAuthored` store or entity-lists file.
    */
  def ownFrontMatter(markup: MarkupPage): Option[FrontMatter] =
    val fullOwnsFile: Boolean =
      markup.isInstanceOf[FullMarkupPage] && markup.source.isDefined
    val authoredOnSynthetic: Boolean =
      !markup.isInstanceOf[FullMarkupPage] && markup.content.exists(isAuthored)
    Option.when(fullOwnsFile || authoredOnSynthetic)(markup.frontMatter)

  /** Inline-list front matter. `/tags` and category membership only.
    * `from` is the catalog. `fromInline` is true. Not chips. Not graph node `tags`. Not `Seo.author`.
    */
  def inlineFrontMatter(markup: MarkupPage): Option[FrontMatter] =
    markup.inlineListProse.map(_.content.frontMatter)
