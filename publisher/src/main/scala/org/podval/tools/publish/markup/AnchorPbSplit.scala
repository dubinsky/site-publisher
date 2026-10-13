package org.podval.tools.publish.markup

import org.podval.xml.{Xml, XmlAttribute, XmlName}

/** Lift a descendant `a.pb` out of an anchor so the facsimile link is not nested. */
private[markup] object AnchorPbSplit:
  private enum Piece:
    case Content(nodes: Seq[Xml.Node])
    case Break(pb: Xml.Element)

  def lift(element: Xml.Element): Seq[Xml.Node] =
    if Pb.is(element) || !containsPb(element) then Seq(element)
    else
      val children: Seq[Xml.Node] = element.getChildren.flatMap(liftNode)
      if splittableAnchor(element) then split(element, children)
      else Seq(element.setChildren(children))

  private def liftNode(node: Xml.Node): Seq[Xml.Node] =
    node.asElement.fold(Seq(node))(lift)

  private def splittableAnchor(element: Xml.Element): Boolean =
    element.isA && !Pb.is(element)

  /** Descendant `a.pb`, not this element itself. */
  private def containsPb(element: Xml.Element): Boolean =
    element.childElements.exists(child => Pb.is(child) || containsPb(child))

  private def split(anchor: Xml.Element, children: Seq[Xml.Node]): Seq[Xml.Node] =
    emit(anchor, coalesce(children.flatMap(explode)))

  private def explode(node: Xml.Node): Seq[Piece] = node.asElement match
    case None =>
      Seq(Piece.Content(Seq(node)))
    case Some(element) if Pb.is(element) =>
      Seq(Piece.Break(element))
    case Some(element) if !containsPb(element) =>
      Seq(Piece.Content(Seq(element)))
    case Some(element) if splittableAnchor(element) =>
      split(element, element.getChildren.flatMap(liftNode)).flatMap(explode)
    case Some(element) =>
      wrapRuns(element, coalesce(element.getChildren.flatMap(explode)))

  private def coalesce(pieces: Seq[Piece]): Seq[Piece] =
    pieces.foldLeft(Seq.empty[Piece]):
      case (init :+ Piece.Content(prev), Piece.Content(next)) =>
        init :+ Piece.Content(prev ++ next)
      case (acc, piece) =>
        acc :+ piece

  private def wrapRuns(element: Xml.Element, pieces: Seq[Piece]): Seq[Piece] =
    pieces.foldLeft((true, Seq.empty[Piece])):
      case ((keepIds, acc), Piece.Break(pb)) =>
        (keepIds, acc :+ Piece.Break(pb))
      case ((keepIds, acc), Piece.Content(nodes)) if isBlank(nodes) =>
        (keepIds, acc)
      case ((keepIds, acc), Piece.Content(nodes)) =>
        (false, acc :+ Piece.Content(Seq(cloneContent(element, nodes, keepIds))))
    ._2

  private def emit(element: Xml.Element, pieces: Seq[Piece]): Seq[Xml.Node] =
    pieces.foldLeft((true, Seq.empty[Xml.Node])):
      case ((keepIds, acc), Piece.Break(pb)) =>
        (keepIds, acc :+ pb)
      case ((keepIds, acc), Piece.Content(nodes)) if isBlank(nodes) =>
        (keepIds, acc)
      case ((keepIds, acc), Piece.Content(nodes)) =>
        (false, acc :+ cloneContent(element, nodes, keepIds))
    ._2

  private def cloneContent(element: Xml.Element, children: Seq[Xml.Node], keepIds: Boolean): Xml.Element =
    val cloned: Xml.Element = element.setChildren(children)
    if keepIds then cloned else dropIds(cloned)

  private def dropIds(element: Xml.Element): Xml.Element =
    element.setAttributes(element.getAttributes.filterNot((name, _) => isId(name)))

  private def isId(name: XmlName): Boolean =
    XmlAttribute.Id.matches(name) || XmlAttribute.XmlId.matches(name)

  private def isBlank(nodes: Seq[Xml.Node]): Boolean =
    nodes.forall(_.isWhitespace)
