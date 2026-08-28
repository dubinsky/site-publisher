package org.podval.tools.publish.page

import org.podval.xml.Html
import zio.blocks.html.{nav as navEl, *}

/** Batches for the synthetic posts listing. */
object PagedList:
  def batchCount(items: Int, size: Int): Int =
    if size < 1 || items <= 0 then 1
    else (items + size - 1) / size

  def slice[A](items: Seq[A], pageIndex: Int, size: Int): Seq[A] =
    items.drop((pageIndex - 1) * size).take(size)

  def nav(current: Int, total: Int, hrefFor: Int => String): Html.Element =
    def pageLink(i: Int, label: String, cls: String): Html.Element =
      a(className := cls, href := hrefFor(i), label)
    navEl(className := "pagination", aria("label") := "Pagination",
      Option.when(current > 1)(pageLink(current - 1, "Newer", "pagination-prev")),
      (1 to total).map: i =>
        if i == current
        then span(className := "pagination-current", aria("current") := "page", i.toString)
        else pageLink(i, i.toString, "pagination-page")
      ,
      Option.when(current < total)(pageLink(current + 1, "Older", "pagination-next"))
    )

