package org.podval.store

import org.podval.metadata.HasValues

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

trait Stores[+T <: Store] extends Store:
  def stores: Seq[T]

  def findByName(name: String): Option[T] = HasValues.find(stores, name)

  // TODO add indexOf() and friends

  /*
  Successful `resolve()` returns a 'path' - a sequence of `Stores`;
  URL resolved by such a path can be reconstructed from it (in its canonical form);
  such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

  Store.Path returned is nonEmpty ;)
 */
  final def resolve(path: String): Path = resolve(Stores.splitAndDecodeUrl(path))

  // TODO does this work with an alias "/" - and should such alias be legal?
  final def resolve(path: Seq[String]): Path =
    if path.nonEmpty then this.resolve(path, Seq.empty) else Seq(this)

  private def resolve(
    path: Seq[String],
    acc: Path
  ): Path = if path.isEmpty then acc.reverse else
    val head: String = path.head
    val tail: Seq[String] = path.tail
    val nextOpt: Option[Store] = findByName(head)
    require(nextOpt.nonEmpty, s"Did not find '$head' in $this")
    nextOpt.get match
      case alias: Alias =>
        val toPath: Path = resolve(alias.to)
        val stores: Stores[?] = Path.last[Stores[?]](toPath)
        stores.resolve(tail, toPath.reverse ++ acc)
      case stores: Stores[?] => stores.resolve(tail, stores +: acc)
      case next =>
        require(tail.isEmpty, s"Can not apply '$tail' to $next")
        (next +: acc).reverse

object Stores:
  private def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    val startsWithSlash: Boolean = url.startsWith("/")
    // TODO? require(startsWithSlash)
    (if startsWithSlash then url.substring(1) else url).split("/").toIndexedSeq.filterNot(_.isBlank)

  private def splitAndDecodeUrl(url: String): Seq[String] =
    splitUrl(url).map(segment => URLDecoder.decode(segment, StandardCharsets.UTF_8))
