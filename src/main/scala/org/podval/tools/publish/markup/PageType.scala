package org.podval.tools.publish.markup

/** Collection `pageType`: manuscript recto/verso names (`000` / `000об`) vs book page numbers. */
enum PageType derives CanEqual:
  case Manuscript, Book

  def displayName(n: String): String = this match
    case PageType.Book => n
    case PageType.Manuscript => PageType.manuscriptDisplayName(n).getOrElse(n)

object PageType:
  val defaultName: String = "manuscript"
  val bookName: String = "book"

  def parse(value: Option[String]): PageType =
    value.map(_.trim).filter(_.nonEmpty) match
      case Some(name) if name == bookName => Book
      case _ => Manuscript

  def isKnown(value: String): Boolean =
    value == defaultName || value == bookName

  private val frontSuffix: String = "-1"
  private val backSuffix: String = "-2"
  private val numberOfDigitsInName: Int = 3

  private def manuscriptDisplayName(n: String): Option[String] =
    val suffix: Option[(String, Boolean)] =
      if n.endsWith(frontSuffix) then Some((n.dropRight(frontSuffix.length), false))
      else if n.endsWith(backSuffix) then Some((n.dropRight(backSuffix.length), true))
      else None
    suffix.flatMap: (base, back) =>
      val digits: Int = base.takeWhile(_.isDigit).length
      val rest: String = base.drop(digits)
      Option.when(digits >= numberOfDigitsInName && (rest.isEmpty || rest == "a")):
        base + (if back then "об" else "")
