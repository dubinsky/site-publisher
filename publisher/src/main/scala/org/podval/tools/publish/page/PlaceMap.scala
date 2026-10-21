package org.podval.tools.publish.page

import org.podval.metadata.Language
import org.podval.tools.publish.markup.EntityKind
import org.podval.tools.publish.site.Messages
import org.podval.tools.publish.util.Files
import org.podval.xml.{Xml, XmlElement}
import java.io.File
import java.text.Collator
import java.util.Locale
import scala.annotation.tailrec

/** The `/map` page draws this when any place has a TEI `geo` (latitude, then longitude).
  * Circle area follows how many cases name the place. A case is a collection whose parent
  * groups by `case` or `дело`; leaves of one case count once. */
object PlaceMap:
  val maxSpanDegrees: Double = 15

  final case class Place(
    name: String,
    href: String,
    target: Option[String],
    lat: Double,
    lon: Double,
    cases: Int
  ) derives CanEqual

  private val width: Double = 640
  private val gutter: Double = 28
  private val band: Double = 44
  private val minRadius: Double = 4.5
  private val maxRadius: Double = 18
  private val kmPerDegree: Double = 111.32

  def parseGeo(text: String): Option[(Double, Double)] =
    text.trim.split("\\s+").toList match
      case latText :: lonText :: Nil =>
        for
          lat <- latText.toDoubleOption
          lon <- lonText.toDoubleOption
          if lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180
        yield (lat, lon)
      case _ => None

  /** Area follows the count. The radius follows the square root, and a small count stays visible. */
  def radius(cases: Int, maxCases: Int): Double =
    if cases <= 0 || maxCases <= 0 then minRadius
    else math.max(minRadius, maxRadius * math.sqrt(cases.toDouble / maxCases))

  /** Keeps the cluster that fits in [[maxSpanDegrees]]. Far places stay in the table. */
  def partition(places: Seq[Place]): (Seq[Place], Seq[Place]) =
    val inside: Seq[Place] = keep(places)
    (inside, places.filterNot(inside.contains))

  /** Places with a parseable `geo`. The emit check; case counts stay in [[render]]. */
  def located(pages: Seq[Page]): Seq[Page] =
    pages.filter(page => page.entityKind.contains(EntityKind.Place) && geoOf(page).isDefined)

  def render(members: Seq[Page]): Option[Xml.Element] =
    val places: Seq[Place] = placesOf(members)
    Option.when(places.nonEmpty):
      given Language.Spec = members.head.site.languageSpec
      figure(places, Base.load(members.head.site.sourceDirectory))

  private def placesOf(members: Seq[Page]): Seq[Place] =
    val counts: Map[Page, Int] = caseCounts(members)
    members.flatMap: page =>
      geoOf(page).map: (lat, lon) =>
        Place(
          name = EntityLists.displayName(page),
          href = page.real.publishedPath.toString,
          target = NamedWindows.targetAttr(page),
          lat = lat,
          lon = lon,
          cases = counts.getOrElse(page, 0)
        )

  private def geoOf(page: Page): Option[(Double, Double)] =
    page.doc.toSeq.flatMap(_.xml.gather(element =>
      Option.when(element.isNamed("geo"))(element.getText)
    )).flatMap(parseGeo).headOption

  private def caseCounts(members: Seq[Page]): Map[Page, Int] =
    if members.isEmpty then Map.empty
    else
      val wanted: Set[Page] = members.toSet
      members.head.site.backLinks.all
        .filter(link => wanted.contains(link.to.page))
        .groupMap(_.to.page)(identity)
        .view
        .mapValues(links => links.flatMap(link => caseOf(link.from)).distinct.length)
        .toMap

  @tailrec
  private def caseOf(page: Page): Option[Page] =
    if isCase(page) then Some(page)
    else page.parent match
      case Some(parent) => caseOf(parent)
      case None => None

  private def isCase(page: Page): Boolean =
    page.store.exists(_.isCollection) &&
      page.parent.flatMap(_.store).flatMap(_.selector).exists: selector =>
        val name: String = selector.trim
        name == "case" || name == "дело"

  private def keep(places: Seq[Place]): Seq[Place] =
    if places.length <= 1 || fits(places) then places
    else keep(places.filterNot(_ == farthestPlace(places)))

  private def fits(places: Seq[Place]): Boolean =
    places.map(_.lat).max - places.map(_.lat).min <= maxSpanDegrees &&
      places.map(_.lon).max - places.map(_.lon).min <= maxSpanDegrees

  private def farthestPlace(places: Seq[Place]): Place =
    val lat0: Double = median(places.map(_.lat))
    val lon0: Double = median(places.map(_.lon))
    val scale: Double = math.cos(lat0.toRadians)
    places.maxBy: place =>
      val dy: Double = place.lat - lat0
      val dx: Double = (place.lon - lon0) * scale
      dx * dx + dy * dy

  private def median(values: Seq[Double]): Double =
    val sorted: Seq[Double] = values.sorted
    val mid: Int = sorted.length / 2
    if sorted.length % 2 == 1 then sorted(mid) else (sorted(mid - 1) + sorted(mid)) / 2

  private def figure(places: Seq[Place], lines: Seq[Base.Line])(using Language.Spec): Xml.Element =
    val (inside, outside) = partition(places)
    val drawn: Seq[Dot] = layout(inside)
    val svg: Xml.Element = drawing(drawn, inside, lines)
    val note: Xml.Element = Xml.element("figcaption").setText(caption(lines.nonEmpty))
    val rows: Xml.Element = table(places, outside.toSet)
    Xml.element("figure").addClass("place-map").setChildren(Seq(svg, note, rows))

  private final case class Dot(place: Place, x: Double, y: Double, r: Double) derives CanEqual

  private final case class Box(x: Double, y: Double, w: Double, h: Double):
    def intersects(other: Box): Boolean =
      x < other.x + other.w && other.x < x + w && y < other.y + other.h && other.y < y + h

    def inside(right: Double, bottom: Double): Boolean =
      x >= 4 && y >= 4 && x + w <= right - 4 && y + h <= bottom - 4

  private final case class Frame(
    latMin: Double,
    latMax: Double,
    lonMin: Double,
    lonMax: Double,
    mapHeight: Double,
    originX: Double,
    originY: Double,
    scale: Double,
    cos: Double
  ):
    def xOf(lon: Double): Double = originX + (lon - lonMin) * cos * scale
    def yOf(lat: Double): Double = originY + (latMax - lat) * scale

  private def layout(inside: Seq[Place]): Seq[Dot] =
    val frame: Frame = frameOf(inside)
    val maxCases: Int = inside.map(_.cases).max
    val dots: Seq[Dot] = inside.map: place =>
      val r: Double = if place.cases == 0 then minRadius else radius(place.cases, maxCases)
      Dot(place, frame.xOf(place.lon), frame.yOf(place.lat), r)
    dots.sortBy(-_.place.cases)

  private def frameOf(inside: Seq[Place]): Frame =
    val midLat: Double = inside.map(_.lat).sum / inside.length
    val cos: Double = math.max(0.2, math.cos(midLat.toRadians))
    val padLat: Double = 0.7
    val padLon: Double = padLat / cos
    val latMin: Double = inside.map(_.lat).min - padLat
    val latMax: Double = inside.map(_.lat).max + padLat
    val lonMin: Double = inside.map(_.lon).min - padLon
    val lonMax: Double = inside.map(_.lon).max + padLon
    val spanLon: Double = math.max((lonMax - lonMin) * cos, 0.5)
    val spanLat: Double = math.max(latMax - latMin, 0.5)
    val innerW: Double = width - 2 * gutter
    val rawH: Double = innerW * spanLat / spanLon
    val mapH: Double = math.max(rawH, 320)
    val scale: Double = math.min(innerW / spanLon, (mapH - 2 * gutter) / spanLat)
    val usedW: Double = spanLon * scale
    val usedH: Double = spanLat * scale
    Frame(
      latMin, latMax, lonMin, lonMax, mapH,
      originX = gutter + (innerW - usedW) / 2,
      originY = gutter + (mapH - 2 * gutter - usedH) / 2,
      scale, cos
    )

  private def drawing(dots: Seq[Dot], inside: Seq[Place], lines: Seq[Base.Line])(using Language.Spec): Xml.Element =
    val frame: Frame = frameOf(inside)
    val height: Double = frame.mapHeight + band
    val clip: Xml.Element = Xml.element("clipPath").set("id", "place-map-frame").setChildren(Seq(
      Xml.element("rect").set("width", fmt(width)).set("height", fmt(frame.mapHeight))
    ))
    val clipped: Xml.Element = Xml.element("g").set("clip-path", "url(#place-map-frame)").setChildren(
      Seq(frameRect(frame), graticule(frame)) ++ basePaths(frame, lines) ++ Seq(dotsOf(dots))
    )
    val labels: Xml.Element = Xml.element("g").addClass("place-map-labels")
      .set("pointer-events", "none")
      .setChildren(labelTexts(dots, frame.mapHeight))
    Xml.element("svg")
      .set("viewBox", s"0 0 ${fmt(width)} ${fmt(height)}")
      .set("role", "img")
      .setChildren(Seq(
        Xml.element("title").setText(Messages.text("map.title")),
        Xml.element("defs").setChildren(Seq(clip)),
        clipped,
        labels,
        north,
        legend(dots, frame),
        scaleBar(frame)
      ))

  private def frameRect(frame: Frame): Xml.Element =
    Xml.element("rect").addClass("place-map-frame")
      .set("x", "0").set("y", "0")
      .set("width", fmt(width)).set("height", fmt(frame.mapHeight))

  private def graticule(frame: Frame): Xml.Element =
    val lines: Seq[Xml.Element] =
      ticks(frame.latMin, frame.latMax).map: lat =>
        line(frame.xOf(frame.lonMin), frame.yOf(lat), frame.xOf(frame.lonMax), frame.yOf(lat))
      ++ ticks(frame.lonMin, frame.lonMax).map: lon =>
        line(frame.xOf(lon), frame.yOf(frame.latMin), frame.xOf(lon), frame.yOf(frame.latMax))
    Xml.element("g").addClass("place-map-graticule").setChildren(lines)

  private def ticks(min: Double, max: Double): Seq[Int] =
    val step: Int = if max - min > 8 then 2 else 1
    val start: Int = math.ceil(min / step).toInt * step
    Iterator.iterate(start)(_ + step).takeWhile(_ < max).toSeq

  private def basePaths(frame: Frame, lines: Seq[Base.Line]): Seq[Xml.Element] =
    Seq(
      ("river", "place-map-river", "place-map-rivers"),
      ("coast", "place-map-coast", "place-map-coasts")
    ).flatMap: (kind, pathClass, groupClass) =>
      val paths: Seq[Xml.Element] = lines.filter(_.kind == kind).flatMap(pathOf(frame, pathClass))
      Option.when(paths.nonEmpty)(Xml.element("g").addClass(groupClass).setChildren(paths))

  private def pathOf(frame: Frame, css: String)(line: Base.Line): Option[Xml.Element] =
    val points: Seq[(Double, Double)] = line.points.map((lon, lat) => (frame.xOf(lon), frame.yOf(lat)))
    Option.when(points.length >= 2):
      val d: String = points.map((x, y) => s"${fmt(x)} ${fmt(y)}").mkString("M ", " L ", "")
      Xml.element("path").addClass(css).set("d", d)

  private def dotsOf(dots: Seq[Dot]): Xml.Element =
    Xml.element("g").addClass("place-map-dots").setChildren(dots.map: dot =>
      val circle: Xml.Element = Xml.element("circle")
        .addClass(if dot.place.cases == 0 then "place-map-ring" else "place-map-dot")
        .set("cx", fmt(dot.x)).set("cy", fmt(dot.y)).set("r", fmt(dot.r))
      val tip: Xml.Element = Xml.element("title").setText(s"${dot.place.name}, ${dot.place.cases}")
      val link: Xml.Element = Xml.element(XmlElement.A).setHref(dot.place.href).setChildren(Seq(circle, tip))
      dot.place.target.fold(link)(name => link.set("target", name))
    )

  private def labelTexts(dots: Seq[Dot], mapHeight: Double): Seq[Xml.Element] =
    val circleBoxes: Seq[(Dot, Box)] = dots.map(dot => dot -> Box(dot.x - dot.r, dot.y - dot.r, dot.r * 2, dot.r * 2))
    val placed: Seq[(Dot, Box)] = dots.foldLeft(Seq.empty[(Dot, Box)]): (acc, dot) =>
      val own: Box = circleBoxes.find(_._1 == dot).get._2
      val others: Seq[Box] = circleBoxes.collect { case (other, box) if other != dot => box }
      labelCandidates(dot).find(box =>
        box.inside(width, mapHeight) &&
          acc.forall(placed => !box.intersects(placed._2)) &&
          others.forall(other => !box.intersects(other)) &&
          !box.intersects(own)
      ) match
        case Some(box) => acc :+ (dot, box)
        case None => acc
    placed.map: (dot, box) =>
      Xml.element("text").addClass("place-map-label")
        .set("x", fmt(box.x)).set("y", fmt(box.y + box.h - 3))
        .setText(dot.place.name)

  private def labelCandidates(dot: Dot): Seq[Box] =
    val w: Double = 7.2 * dot.place.name.length
    val h: Double = 14
    val gap: Double = 4
    Seq(
      Box(dot.x + dot.r + gap, dot.y - h / 2, w, h),
      Box(dot.x - dot.r - gap - w, dot.y - h / 2, w, h),
      Box(dot.x - w / 2, dot.y - dot.r - gap - h, w, h),
      Box(dot.x - w / 2, dot.y + dot.r + gap, w, h)
    )

  private def north(using Language.Spec): Xml.Element =
    val x: Double = width - 22
    val y: Double = 16
    Xml.element("g").addClass("place-map-north").setChildren(Seq(
      line(x, y + 22, x, y + 6),
      Xml.element("polygon").set("points", s"$x,${fmt(y)} ${fmt(x - 4)},${fmt(y + 8)} ${fmt(x + 4)},${fmt(y + 8)}"),
      Xml.element("text").set("x", fmt(x)).set("y", fmt(y + 34)).set("text-anchor", "middle")
        .setText(Messages.text("map.north"))
    ))

  private def legend(dots: Seq[Dot], frame: Frame)(using Language.Spec): Xml.Element =
    val maxCases: Int = dots.map(_.place.cases).maxOption.getOrElse(0)
    val counts: Seq[Int] = legendCounts(dots.map(_.place.cases))
    val y: Double = frame.mapHeight + 26
    val items: Seq[Xml.Element] = counts.zipWithIndex.flatMap: (count, index) =>
      val x: Double = 24 + index * 92
      val r: Double = if count == 0 then minRadius else radius(count, maxCases)
      val circle: Xml.Element = Xml.element("circle")
        .addClass(if count == 0 then "place-map-ring" else "place-map-dot")
        .set("cx", fmt(x)).set("cy", fmt(y)).set("r", fmt(r))
      val text: Xml.Element = Xml.element("text").addClass("place-map-label")
        .set("x", fmt(x + r + 6)).set("y", fmt(y + 4))
        .setText(if count == 0 then Messages.text("map.noCases") else count.toString)
      Seq(circle, text)
    Xml.element("g").addClass("place-map-legend").setChildren(items)

  private def legendCounts(counts: Seq[Int]): Seq[Int] =
    val positive: Seq[Int] = counts.filter(_ > 0).distinct.sorted
    val sized: Seq[Int] = positive match
      case Seq() => Seq.empty
      case Seq(one) => Seq(one)
      case many => Seq(many.head, many(many.length / 2), many.last).distinct
    if counts.exists(_ == 0) then sized :+ 0 else sized

  private def scaleBar(frame: Frame)(using Language.Spec): Xml.Element =
    val pxPerKm: Double = frame.scale / kmPerDegree
    val km: Int = Seq(50, 100, 200, 400).find(k =>
      val px = k * pxPerKm
      px >= 48 && px <= 160
    ).getOrElse(100)
    val bar: Double = km * pxPerKm
    val x: Double = width - gutter - bar
    val y: Double = frame.mapHeight + 26
    Xml.element("g").addClass("place-map-scale").setChildren(Seq(
      line(x, y, x + bar, y),
      line(x, y - 4, x, y + 4),
      line(x + bar, y - 4, x + bar, y + 4),
      Xml.element("text").addClass("place-map-label")
        .set("x", fmt(x)).set("y", fmt(y - 8))
        .setText(Messages.text("map.km", "km" -> km))
    ))

  private def line(x1: Double, y1: Double, x2: Double, y2: Double): Xml.Element =
    Xml.element("line")
      .set("x1", fmt(x1)).set("y1", fmt(y1))
      .set("x2", fmt(x2)).set("y2", fmt(y2))

  private def table(places: Seq[Place], outside: Set[Place])(using spec: Language.Spec): Xml.Element =
    val collator: Collator = Collator.getInstance(
      if spec.language.contains(Language.Russian) then Locale.forLanguageTag("ru") else Locale.ROOT
    )
    val ordered: Seq[Place] = places.sortWith: (a, b) =>
      if a.cases != b.cases then a.cases > b.cases
      else collator.compare(a.name, b.name) < 0
    val head: Seq[String] =
      Seq(Messages.text("map.place"), Messages.text("map.cases")) ++
        Option.when(outside.nonEmpty)("").toSeq
    val header: Xml.Element = Xml.element(XmlElement.Tr).setChildren(
      head.map(label => Xml.element(XmlElement.Th).setText(label))
    )
    val rows: Seq[Xml.Element] = ordered.map: place =>
      val link: Xml.Element = Xml.element(XmlElement.A).setHref(place.href).setText(place.name)
      val linked: Xml.Element = place.target.fold(link)(name => link.set("target", name))
      val cells: Seq[Xml.Element] = Seq(
        Xml.element(XmlElement.Td).setChildren(Seq(linked)),
        Xml.element(XmlElement.Td).setText(place.cases.toString)
      ) ++ Option.when(outside.nonEmpty)(
        Xml.element(XmlElement.Td).setText(if outside.contains(place) then Messages.text("map.outside") else "")
      ).toSeq
      Xml.element(XmlElement.Tr).setChildren(cells)
    Xml.element(XmlElement.Table).addClass("place-map-table").setChildren(header +: rows)

  private def fmt(n: Double): String = String.format(Locale.US, "%.1f", n)

  private def caption(withLines: Boolean)(using Language.Spec): String =
    val lines: String = if !withLines then "" else Messages.text("map.lines") + " "
    Messages.text("map.caption", "lines" -> lines)

  private object Base:
    final case class Line(kind: String, points: Seq[(Double, Double)])

    val fileName: String = "_place-map-base.txt"

    /** Site file. The publisher ships no rivers or coast. A missing file draws the map without them. */
    def load(sourceDirectory: File): Seq[Line] =
      val file: File = File(sourceDirectory, fileName)
      if !file.isFile then Seq.empty
      else Files.read(file).split("\n\\s*\n").toSeq.flatMap(parseBlock)

    private def parseBlock(block: String): Option[Line] =
      val rows: Seq[String] = block.linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
      val points: Seq[(Double, Double)] = rows.dropWhile(_.startsWith("#")).flatMap: row =>
        row.split("\\s+").toList match
          case lon :: lat :: Nil =>
            for
              x <- lon.toDoubleOption
              y <- lat.toDoubleOption
            yield (x, y)
          case _ => None
      val kind: String = if rows.headOption.exists(_.startsWith("# coast")) then "coast" else "river"
      Option.when(points.length >= 2)(Line(kind, points))
