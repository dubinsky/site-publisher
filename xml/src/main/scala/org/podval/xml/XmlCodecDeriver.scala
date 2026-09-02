package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.docs.Doc
import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset
import zio.blocks.schema.derive.{BindingInstance, Deriver, InstanceOverride, InstanceOverrideByType}
import zio.blocks.typeid.TypeId
import scala.annotation.switch
import scala.collection.mutable
import scala.reflect.ClassTag

object XmlCodecDeriver extends XmlCodecDeriver:
  def tagged[A, K](tagField: String, tag: XmlTag[K])(using typeId: TypeId[A]): XmlCodecDeriver =
    val target: String = typeId.fullName
    val xmlTag: XmlTag[Any] = tag.erased
    new XmlCodecDeriver:
      override protected def tagBinding(id: TypeId[?]): Option[(String, XmlTag[Any])] =
        Option.when(id.fullName == target)((tagField, xmlTag))

class XmlCodecDeriver extends Deriver[XmlCodec]:
  protected def tagBinding(typeId: TypeId[?]): Option[(String, XmlTag[Any])] = None

  override def derivePrimitive[A](
    primitiveType: PrimitiveType[A],
    typeId: TypeId[A],
    binding: Binding.Primitive[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  ): Lazy[XmlCodec[A]] =
    if binding.isInstanceOf[Binding[?, ?]] then Lazy(primitiveCodec(primitiveType))
    else binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance

  override def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Record[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else Lazy:
      val recordBinding: Binding.Record[A] = binding
      val isRecursive: Boolean = fields.exists(_.value.isInstanceOf[Reflect.Deferred[F, ?]])
      var fieldInfos: Array[FieldInfo] =
        if isRecursive then recursiveRecordCache.get.get(typeId) else null
      val deriveCodecs: Boolean = fieldInfos eq null
      if deriveCodecs then
        fieldInfos = new Array[FieldInfo](fields.length)
        if isRecursive then recursiveRecordCache.get.put(typeId, fieldInfos)
        var offset: RegisterOffset = 0L
        var idx: Int = 0
        while idx < fields.length do
          val field: Term[F, A, ?] = fields(idx)
          fieldInfos(idx) = fieldInfo(typeId, field, offset)
          offset = RegisterOffset.add(registerOffset(field.value), offset)
          idx += 1
      new RecordCodec[A](
        typeId = typeId,
        modifiers = modifiers,
        fieldInfos = fieldInfos,
        constructor = recordBinding.constructor,
        deconstructor = recordBinding.deconstructor,
        xmlTag = tagBinding(typeId).map(_._2)
      )

  override def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Variant[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else if typeId.isOption then
      val inner: Reflect[F, ?] = cases(1).value.asRecord.get.fields(0).value
      D.instance(inner.metadata).map: codec =>
        val innerCodec: XmlCodec[Any] = codec.asInstanceOf[XmlCodec[Any]]
        new XmlCodec[Option[Any]]:
          override def elementName: String = innerCodec.elementName
          override def isRecordLike: Boolean = innerCodec.isRecordLike
          override def caseNames: Seq[String] = innerCodec.caseNames
          override def isEnumeration: Boolean = innerCodec.isEnumeration
          override def unsafeDecode[E: XmlAst](element: E): Option[Any] =
            Some(innerCodec.unsafeDecode(element))
          override def encodeNamed[E: XmlAst](name: String, value: Option[Any]): E = value match
            case Some(innerValue) => innerCodec.encodeNamed(name, innerValue)
            case None => throw XmlError("Cannot encode None as an element")
          override def unsafeDecodeText(text: String): Option[Any] =
            Some(innerCodec.unsafeDecodeText(text))
          override def encodeText(value: Option[Any]): String = value match
            case Some(innerValue) => innerCodec.encodeText(innerValue)
            case None => throw XmlError("Cannot encode None as text")
        .asInstanceOf[XmlCodec[A]]
    else Lazy:
      val caseCodecs: IndexedSeq[(String, XmlCodec[A], Option[A])] = cases.map: caseTerm =>
        val name: String = configuredElementName(caseTerm.name, caseTerm.modifiers, caseTerm.value.modifiers)
        val codec: XmlCodec[A] = D.instance(caseTerm.value.metadata).force.asInstanceOf[XmlCodec[A]]
        val empty: Option[A] = caseTerm.value.asRecord.filter(_.fields.isEmpty).map: record =>
          val ctor: Constructor[?] = F.record(record.recordBinding).constructor
          ctor.construct(Registers(ctor.usedRegisters), 0).asInstanceOf[A]
        (name, codec, empty)
      val enumeration: Boolean = caseCodecs.forall(_._3.isDefined)
      val discriminator: Discriminator[A] = binding.discriminator
      def caseByName(name: String): Option[(String, XmlCodec[A], Option[A])] =
        caseCodecs.find((caseName, _, _) => namesMatch(name, caseName))
      new XmlCodec[A]:
        override def elementName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
        override def isRecordLike: Boolean = true
        override def isEnumeration: Boolean = enumeration
        override def caseNames: Seq[String] = caseCodecs.map(_._1)
        override def unsafeDecode[E: XmlAst](element: E): A =
          val name: String = summon[XmlAst[E]].getName(element)
          caseByName(name) match
            case Some((_, codec, empty)) =>
              empty.getOrElse(codec.unsafeDecode(element))
            case None if enumeration =>
              unsafeDecodeText(characterData(element))
            case None => throw XmlError(s"Unknown variant case: $name")
        override def encodeNamed[E: XmlAst](name: String, value: A): E =
          val idx: Int = discriminator.discriminate(value)
          val (caseName, codec, _) = caseCodecs(idx)
          if enumeration then
            val ast: XmlAst[E] = summon[XmlAst[E]]
            ast.element(name, Seq.empty, Seq(ast.text(caseName)))
          else codec.encodeNamed(caseName, value)
        override def encode[E: XmlAst](value: A): E =
          val idx: Int = discriminator.discriminate(value)
          val (caseName, codec, _) = caseCodecs(idx)
          if enumeration then summon[XmlAst[E]].element(caseName, Seq.empty, Seq.empty)
          else codec.encodeNamed(caseName, value)
        override def unsafeDecodeText(text: String): A =
          if !enumeration then throw XmlError("Variant does not decode from text")
          caseCodecs.find((caseName, _, _) => caseName == text.trim) match
            case Some((_, _, Some(value))) => value
            case _ => throw XmlError(s"Unknown enumeration value: $text")
        override def encodeText(value: A): String =
          if !enumeration then throw XmlError("Variant does not encode as text")
          caseCodecs(discriminator.discriminate(value))._1

  override def deriveSequence[F[_, _], C[_], A](
    element: Reflect[F, A],
    typeId: TypeId[C[A]],
    binding: Binding.Seq[C, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[C[A]],
    examples: Seq[C[A]]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[C[A]]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, C[A]]].instance
    else
      val seqBinding: Binding.Seq[C, A] = binding
      val itemClassTag: ClassTag[A] = element.typeId.classTag.asInstanceOf[ClassTag[A]]
      D.instance(element.metadata).map: codec =>
        val itemCodec: XmlCodec[A] = codec
        new XmlCodec[C[A]]:
          override def elementName: String = typeId.name
          override def unsafeDecode[E: XmlAst](root: E): C[A] =
            val ast: XmlAst[E] = summon[XmlAst[E]]
            val children: Seq[E] = ast.getChildren(root).flatMap(_.asElement)
            val names: Seq[String] =
              if itemCodec.caseNames.nonEmpty then itemCodec.caseNames else Seq(itemCodec.elementName)
            val matched: Seq[E] =
              if itemCodec.isRecordLike then children.filter(child => names.exists(namesMatch(ast.getName(child), _)))
              else children
            val builder = seqBinding.constructor.newBuilder[A](matched.size)(using itemClassTag)
            matched.foreach: child =>
              seqBinding.constructor.add(builder, itemCodec.unsafeDecode(child))
            seqBinding.constructor.result(builder)
          override def encodeNamed[E: XmlAst](name: String, value: C[A]): E =
            val ast: XmlAst[E] = summon[XmlAst[E]]
            val items: Iterator[A] = seqBinding.deconstructor.deconstruct(value)
            val children: Seq[ast.Node] = items.map: item =>
              val encoded: E =
                if itemCodec.caseNames.nonEmpty then itemCodec.encode(item)
                else itemCodec.encodeNamed(itemCodec.elementName, item)
              encoded
            .toSeq
            ast.element(name, Seq.empty, children)
          override def encode[E: XmlAst](value: C[A]): E = encodeNamed(elementName, value)

  override def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeId: TypeId[M[K, V]],
    binding: Binding.Map[M, K, V],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[M[K, V]],
    examples: Seq[M[K, V]]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[M[K, V]]] =
    Lazy(unsupported(s"Map ${typeId.name}"))

  override def deriveDynamic[F[_, _]](
    binding: Binding.Dynamic,
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[DynamicValue],
    examples: Seq[DynamicValue]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[DynamicValue]] =
    Lazy(unsupported("DynamicValue"))

  override def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeId: TypeId[A],
    binding: Binding.Wrapper[A, B],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else
      val wrapperBinding: Binding.Wrapper[A, B] = binding
      D.instance(wrapped.metadata).map: codec =>
        val inner: XmlCodec[B] = codec
        new XmlCodec[A]:
          override def elementName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
          override def isRecordLike: Boolean = inner.isRecordLike
          override def caseNames: Seq[String] = inner.caseNames
          override def isEnumeration: Boolean = inner.isEnumeration
          override def unsafeDecode[E: XmlAst](element: E): A = wrapperBinding.wrap(inner.unsafeDecode(element))
          override def encodeNamed[E: XmlAst](name: String, value: A): E =
            inner.encodeNamed(name, wrapperBinding.unwrap(value))
          override def unsafeDecodeText(text: String): A = wrapperBinding.wrap(inner.unsafeDecodeText(text))
          override def encodeText(value: A): String = inner.encodeText(wrapperBinding.unwrap(value))

  override def instanceOverrides: IndexedSeq[InstanceOverride] =
    recursiveRecordCache.remove()
    Chunk(
      InstanceOverrideByType(TypeId.of[XmlNode], Lazy(XmlNode.codec)),
      InstanceOverrideByType(TypeId.of[XmlNode.Element], Lazy(XmlNode.elementCodec)),
      InstanceOverrideByType(TypeId.of[XmlExtras], Lazy(XmlExtras.codec))
    )

  private val recursiveRecordCache: ThreadLocal[java.util.HashMap[TypeId[?], Array[FieldInfo]]] =
    new ThreadLocal[java.util.HashMap[TypeId[?], Array[FieldInfo]]]:
      override def initialValue: java.util.HashMap[TypeId[?], Array[FieldInfo]] = new java.util.HashMap

  private final class RecordCodec[A](
    typeId: TypeId[A],
    modifiers: Seq[Modifier.Reflect],
    fieldInfos: Array[FieldInfo],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    xmlTag: Option[XmlTag[Any]]
  ) extends XmlCodec[A]:
    private val recordName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
    private val namespace: Option[(String, String)] = namespaceOf(modifiers)
    private val tagField: Option[FieldInfo] = fieldInfos.find(_.kind == FieldKind.Tag)

    override def elementName: String = xmlTag.flatMap(_.names.headOption).getOrElse(recordName)
    override def caseNames: Seq[String] = xmlTag.fold(Seq.empty)(_.names)
    override def isRecordLike: Boolean = true

    override def elementNameOf(value: A): String =
      (xmlTag, tagField) match
        case (Some(tag), Some(info)) =>
          val regs: Registers = Registers(deconstructor.usedRegisters)
          deconstructor.deconstruct(regs, 0, value)
          tag.toName(load(regs, info.offset, info.typeTag))
        case _ => recordName

    override def unsafeDecode[E: XmlAst](element: E): A =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      val attrs: mutable.LinkedHashMap[String, String] = mutable.LinkedHashMap.from(ast.getAttributes(element))
      val nodes: ast.Nodes = ast.getChildren(element)
      val available: mutable.BitSet = mutable.BitSet.empty
      nodes.zipWithIndex.foreach: (node, idx) =>
        if node.asElement.isDefined then available += idx
      val regs: Registers = Registers(constructor.usedRegisters)
      var extrasIndex: Int = -1
      var idx: Int = 0
      while idx < fieldInfos.length do
        val info: FieldInfo = fieldInfos(idx)
        try
          info.kind match
            case FieldKind.Tag =>
              val name: String = ast.getName(element)
              xmlTag.flatMap(tag => tag.fromName(localName(name)).orElse(tag.fromName(name))) match
                case Some(k) => store(regs, info.offset, info.typeTag, k)
                case None => throw XmlError(s"Unknown element: $name")
            case FieldKind.Extras => extrasIndex = idx
            case FieldKind.Text =>
              val text: String = characterData(element)
              val value: Any =
                if info.optional then
                  if text.isEmpty then None else Some(info.codec.unsafeDecodeText(text))
                else if text.isEmpty then info.defaultValue.getOrElse(throw XmlError("Missing text content"))
                else info.codec.unsafeDecodeText(text)
              store(regs, info.offset, info.typeTag, value)
            case FieldKind.Attribute(attrName) =>
              attrs.get(attrName).orElse(attrs.collectFirst:
                case (key, value) if namesMatch(key, attrName) => value
              ) match
                case Some(raw) =>
                  attrs.remove(attrName)
                  attrs.keys.filter(key => namesMatch(key, attrName)).toSeq.foreach(attrs.remove)
                  val decoded: Any = info.codec.unsafeDecodeText(raw)
                  store(regs, info.offset, info.typeTag, if info.optional then Some(decoded) else decoded)
                case None =>
                  if info.optional then store(regs, info.offset, info.typeTag, None)
                  else info.defaultValue match
                    case Some(dv) => store(regs, info.offset, info.typeTag, dv)
                    case None => throw XmlError(s"Missing required attribute: $attrName")
            case FieldKind.Child =>
              val matched: Seq[(E, Int)] = nodes.zipWithIndex.flatMap: (node, nodeIdx) =>
                if !available.contains(nodeIdx) then None
                else node.asElement.filter(el => info.itemNames.exists(namesMatch(ast.getName(el), _))).map(_ -> nodeIdx)
              if info.sequence then
                val decodedItems: Seq[Any] = matched.map: (el, nodeIdx) =>
                  available -= nodeIdx
                  info.codec.unsafeDecode(el)
                val seqValue: Any = buildSeq(info, decodedItems)
                val stored: Any =
                  if info.optional then
                    if decodedItems.isEmpty then None else Some(seqValue)
                  else seqValue
                store(regs, info.offset, info.typeTag, stored)
              else if info.optional then
                matched.headOption match
                  case Some((el, nodeIdx)) =>
                    available -= nodeIdx
                    store(regs, info.offset, info.typeTag, Some(info.codec.unsafeDecode(el)))
                  case None => store(regs, info.offset, info.typeTag, None)
              else
                matched.headOption match
                  case Some((el, nodeIdx)) =>
                    available -= nodeIdx
                    store(regs, info.offset, info.typeTag, info.codec.unsafeDecode(el))
                  case None =>
                    info.defaultValue match
                      case Some(dv) => store(regs, info.offset, info.typeTag, dv)
                      case None => throw XmlError(s"Missing required element: ${info.itemNames.mkString("|")}")
        catch
          case e: XmlError => throw e.at(info.fieldName)
        idx += 1

      if extrasIndex >= 0 then
        val leftoverAttrs: Seq[(String, String)] = attrs.iterator.filterNot((key, _) => isXmlns(key)).toSeq
        val hasTextField: Boolean = fieldInfos.exists(_.kind == FieldKind.Text)
        val leftoverNodes: Seq[XmlNode] = nodes.zipWithIndex.flatMap: (node, nodeIdx) =>
          val leftoverElement: Boolean = available.contains(nodeIdx)
          val leftoverText: Boolean = node.asElement.isEmpty && !hasTextField
          if leftoverElement || leftoverText then XmlNode.fromNode(using ast)(node) else None
        store(regs, fieldInfos(extrasIndex).offset, 0, XmlExtras(leftoverAttrs, leftoverNodes))
      else
        val leftoverAttrs: Seq[String] = attrs.keys.iterator.filterNot(isXmlns).toSeq
        if leftoverAttrs.nonEmpty then throw XmlError(s"Unparsed attributes: ${leftoverAttrs.mkString(", ")}")
        val leftoverElements: Seq[String] = nodes.zipWithIndex.flatMap: (node, nodeIdx) =>
          if available.contains(nodeIdx) then node.asElement.map(ast.getName) else None
        if leftoverElements.nonEmpty then throw XmlError(s"Unparsed elements: ${leftoverElements.mkString(", ")}")
        val leftoverText: Boolean = nodes.zipWithIndex.exists: (node, nodeIdx) =>
          !available.contains(nodeIdx) &&
            node.asElement.isEmpty &&
            node.asAtom.exists(_.trim.nonEmpty) &&
            !fieldInfos.exists(_.kind == FieldKind.Text)
        if leftoverText then throw XmlError("Unparsed character content")
      constructor.construct(regs, 0)

    override def encodeNamed[E: XmlAst](name: String, value: A): E =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      val regs: Registers = Registers(deconstructor.usedRegisters)
      deconstructor.deconstruct(regs, 0, value)
      val attributes: mutable.ArrayBuffer[(String, String)] = mutable.ArrayBuffer.empty
      val children: mutable.ArrayBuffer[ast.Node] = mutable.ArrayBuffer.empty
      var extras: XmlExtras = XmlExtras()
      var idx: Int = 0
      while idx < fieldInfos.length do
        val info: FieldInfo = fieldInfos(idx)
        info.kind match
          case FieldKind.Tag => ()
          case FieldKind.Extras =>
            extras = load(regs, info.offset, 0).asInstanceOf[XmlExtras]
          case FieldKind.Text =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            val textOpt: Option[String] =
              if info.optional then loaded.asInstanceOf[Option[Any]].map(info.codec.encodeText)
              else Some(info.codec.encodeText(loaded))
            textOpt.filter(_.nonEmpty).foreach(text => children += ast.text(text))
          case FieldKind.Attribute(attrName) =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            val raw: Option[String] =
              if info.optional then loaded.asInstanceOf[Option[Any]].map(info.codec.encodeText)
              else Some(info.codec.encodeText(loaded))
            raw.foreach(value => attributes += attrName -> value)
          case FieldKind.Child =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            def appendItem(item: Any): Unit =
              val encoded: E =
                if info.codec.caseNames.nonEmpty then info.codec.encode(item)
                else info.codec.encodeNamed(info.itemNames.head, item)
              children += encoded
            if info.sequence then
              val items: Iterator[Any] =
                if info.optional then
                  loaded.asInstanceOf[Option[Any]] match
                    case Some(seq) => deconstructSeq(info, seq)
                    case None => Iterator.empty
                else deconstructSeq(info, loaded)
              items.foreach(appendItem)
            else if info.optional then
              loaded.asInstanceOf[Option[Any]].foreach(appendItem)
            else appendItem(loaded)
        idx += 1
      extras.attributes.foreach(attributes += _)
      extras.children.foreach(node => children += XmlNode.toNode(using ast)(node))
      val nsAttrs: Seq[(String, String)] = namespace match
        case Some((uri, prefix)) if prefix.nonEmpty => Seq(s"xmlns:$prefix" -> uri)
        case Some((uri, _)) => Seq("xmlns" -> uri)
        case None => Seq.empty
      val qualified: String = namespace match
        case Some((_, prefix)) if prefix.nonEmpty && !name.contains(':') => s"$prefix:$name"
        case _ => name
      ast.element(qualified, nsAttrs ++ attributes.toSeq, children.toSeq)

  private def fieldInfo[F[_, _], A](recordTypeId: TypeId[A], field: Term[F, A, ?], offset: RegisterOffset)(using
    F: HasBinding[F],
    D: HasInstance[F]
  ): FieldInfo =
    val reflect: Reflect[F, ?] = field.value
    val optional: Boolean = reflect.isOption
    val innerReflect: Reflect[F, ?] =
      if optional then reflect.optionInnerType.getOrElse(reflect) else reflect
    val sequence: Boolean = innerReflect.isSequence
    val itemReflect: Reflect[F, ?] =
      if sequence then innerReflect.asSequenceUnknown.get.sequence.element else innerReflect
    val codec: XmlCodec[Any] = D.instance(itemReflect.metadata).force.asInstanceOf[XmlCodec[Any]]
    val kind: FieldKind =
      if tagBinding(recordTypeId).exists(_._1 == field.name) then FieldKind.Tag
      else if isExtrasField(field, itemReflect.typeId) then FieldKind.Extras
      else configValue(field.modifiers, XmlCodec.Attribute) match
        case Some(attr) => FieldKind.Attribute(if attr.isEmpty then field.name else attr)
        case None if configValue(field.modifiers, XmlCodec.Text).isDefined => FieldKind.Text
        case None => FieldKind.Child
    val itemName: String =
      configValue(field.modifiers, XmlCodec.Element).filter(_.nonEmpty)
        .orElse(renameOf(field.modifiers))
        .getOrElse:
          if codec.caseNames.nonEmpty then field.name
          else if codec.isRecordLike then codec.elementName
          else field.name
    val aliases: Seq[String] = field.modifiers.collect { case Modifier.alias(name) => name }
    val itemNames: Seq[String] =
      if codec.caseNames.nonEmpty then codec.caseNames else (itemName +: aliases).distinct
    val seqParts: Option[SeqParts] =
      if !sequence then None
      else
        val seqReflect = innerReflect.asSequenceUnknown.get.sequence
        val ctor = seqReflect.seqConstructor
        val dector = seqReflect.seqDeconstructor
        val classTag: ClassTag[Any] = seqReflect.elemClassTag.asInstanceOf[ClassTag[Any]]
        Some(SeqParts(
          fromItems = items =>
            val builder = ctor.newBuilder[Any](items.size)(using classTag)
            items.foreach(item => ctor.add(builder, item))
            ctor.result(builder),
          toItems = value => dector.asInstanceOf[SeqDeconstructor[Seq]].deconstruct(value.asInstanceOf[Seq[Any]])
        ))
    FieldInfo(
      fieldName = field.name,
      kind = kind,
      optional = optional,
      sequence = sequence,
      codec = codec,
      itemNames = itemNames,
      defaultValue = reflect.getDefaultValue,
      offset = offset,
      typeTag = typeTagOf(reflect),
      seqParts = seqParts
    )

  private def buildSeq(info: FieldInfo, items: Seq[Any]): Any =
    info.seqParts.get.fromItems(items)

  private def deconstructSeq(info: FieldInfo, value: Any): Iterator[Any] =
    info.seqParts.get.toItems(value)

  private def isExtrasField(field: Term[?, ?, ?], typeId: TypeId[?]): Boolean =
    configValue(field.modifiers, XmlCodec.Extras).isDefined || typeId.fullName == "org.podval.xml.XmlExtras"

  private def primitiveCodec[A](primitiveType: PrimitiveType[A]): XmlCodec[A] =
    primitiveType match
      case _: PrimitiveType.Unit.type => textCodec("unit", _ => (), _ => "")
      case _: PrimitiveType.Boolean =>
        textCodec[Boolean](
          "boolean",
          parseBoolean,
          value => if value then "true" else "false"
        ).asInstanceOf[XmlCodec[A]]
      case _: PrimitiveType.Byte => textCodec("byte", _.toByte, _.toString)
      case _: PrimitiveType.Short => textCodec("short", _.toShort, _.toString)
      case _: PrimitiveType.Int => textCodec("int", _.toInt, _.toString)
      case _: PrimitiveType.Long => textCodec("long", _.toLong, _.toString)
      case _: PrimitiveType.Float => textCodec("float", _.toFloat, _.toString)
      case _: PrimitiveType.Double => textCodec("double", _.toDouble, _.toString)
      case _: PrimitiveType.Char =>
        textCodec[Char](
          "char",
          text =>
            if text.length == 1 then text.charAt(0)
            else throw XmlError(s"Expected a single character: $text"),
          _.toString
        ).asInstanceOf[XmlCodec[A]]
      case _: PrimitiveType.String => textCodec("string", identity, identity)
      case _: PrimitiveType.BigInt => textCodec("bigInt", BigInt(_), _.toString)
      case _: PrimitiveType.BigDecimal => textCodec("bigDecimal", BigDecimal(_), _.toString)
      case _ => textCodec(primitiveType.typeId.name, parse => throw XmlError(s"Unsupported primitive: $parse"), _.toString)

  private def textCodec[A](name: String, parse: String => A, format: A => String): XmlCodec[A] =
    new XmlCodec[A]:
      override def elementName: String = name
      override def unsafeDecode[E: XmlAst](element: E): A = unsafeDecodeText(characterData(element))
      override def encodeNamed[E: XmlAst](elementName: String, value: A): E =
        val ast: XmlAst[E] = summon[XmlAst[E]]
        val text: String = format(value)
        ast.element(
          elementName,
          Seq.empty,
          if text.isEmpty then Seq.empty else Seq(ast.text(text))
        )
      override def unsafeDecodeText(text: String): A =
        try parse(text)
        catch
          case e: XmlError => throw e
          case _: NumberFormatException => throw XmlError(s"Invalid $name: $text")
          case _: IllegalArgumentException => throw XmlError(s"Invalid $name: $text")
      override def encodeText(value: A): String = format(value)

  private def unsupported[A](what: String): XmlCodec[A] = new XmlCodec[A]:
    override def elementName: String = what
    override def unsafeDecode[E: XmlAst](element: E): A = throw XmlError(s"$what is not supported")
    override def encodeNamed[E: XmlAst](name: String, value: A): E = throw XmlError(s"$what is not supported")

  private def parseBoolean(text: String): Boolean = text.trim.toLowerCase match
    case "true" | "yes" | "1" => true
    case "false" | "no" | "0" => false
    case other => throw XmlError(s"Invalid boolean: $other")

  private def characterData[E: XmlAst](element: E): String =
    val ast: XmlAst[E] = summon[XmlAst[E]]
    ast.getChildren(element).flatMap(node => ast.asAtom(node)).mkString.trim

  private def namesMatch(actual: String, expected: String): Boolean =
    actual == expected || localName(actual) == expected || localName(actual) == localName(expected)

  private def localName(name: String): String =
    val colon: Int = name.lastIndexOf(':')
    if colon < 0 then name else name.substring(colon + 1)

  private def isXmlns(name: String): Boolean = name == "xmlns" || name.startsWith("xmlns:")

  private def configValue(modifiers: Seq[Modifier], key: String): Option[String] =
    modifiers.collectFirst { case Modifier.config(`key`, value) => value }

  private def renameOf(modifiers: Seq[Modifier.Term]): Option[String] =
    modifiers.collectFirst { case Modifier.rename(name) => name }

  private def configuredElementName(
    defaultName: String,
    termModifiers: Seq[Modifier.Term],
    reflectModifiers: Seq[Modifier.Reflect]
  ): String =
    configValue(termModifiers, XmlCodec.Element).filter(_.nonEmpty)
      .orElse(renameOf(termModifiers))
      .orElse(configValue(reflectModifiers, XmlCodec.Element).filter(_.nonEmpty))
      .getOrElse(defaultName)

  private def namespaceOf(modifiers: Seq[Modifier.Reflect]): Option[(String, String)] =
    configValue(modifiers, XmlCodec.NamespaceUri).map: uri =>
      (uri, configValue(modifiers, XmlCodec.NamespacePrefix).getOrElse(""))

  private def typeTagOf[F[_, _], A](reflect: Reflect[F, A]): Int =
    reflect.asPrimitive.map(_.primitiveType) match
      case Some(_: PrimitiveType.Unit.type) => 9
      case Some(_: PrimitiveType.Boolean) => 5
      case Some(_: PrimitiveType.Byte) => 6
      case Some(_: PrimitiveType.Char) => 7
      case Some(_: PrimitiveType.Short) => 8
      case Some(_: PrimitiveType.Float) => 3
      case Some(_: PrimitiveType.Int) => 1
      case Some(_: PrimitiveType.Double) => 4
      case Some(_: PrimitiveType.Long) => 2
      case _ => 0

  private def registerOffset[F[_, _], A](reflect: Reflect[F, A]): RegisterOffset =
    reflect.asPrimitive.map(_.primitiveType) match
      case Some(_: PrimitiveType.Unit.type) => 0L
      case Some(_: PrimitiveType.Boolean) => RegisterOffset.incrementBooleansAndBytes(0L)
      case Some(_: PrimitiveType.Byte) => RegisterOffset.incrementBooleansAndBytes(0L)
      case Some(_: PrimitiveType.Char) => RegisterOffset.incrementCharsAndShorts(0L)
      case Some(_: PrimitiveType.Short) => RegisterOffset.incrementCharsAndShorts(0L)
      case Some(_: PrimitiveType.Float) => RegisterOffset.incrementFloatsAndInts(0L)
      case Some(_: PrimitiveType.Int) => RegisterOffset.incrementFloatsAndInts(0L)
      case Some(_: PrimitiveType.Double) => RegisterOffset.incrementDoublesAndLongs(0L)
      case Some(_: PrimitiveType.Long) => RegisterOffset.incrementDoublesAndLongs(0L)
      case _ => RegisterOffset.incrementObjects(0L)

  private def store(regs: Registers, offset: RegisterOffset, tag: Int, value: Any): Unit =
    (tag: @switch) match
      case 1 => regs.setInt(offset, value.asInstanceOf[Int])
      case 2 => regs.setLong(offset, value.asInstanceOf[Long])
      case 3 => regs.setFloat(offset, value.asInstanceOf[Float])
      case 4 => regs.setDouble(offset, value.asInstanceOf[Double])
      case 5 => regs.setBoolean(offset, value.asInstanceOf[Boolean])
      case 6 => regs.setByte(offset, value.asInstanceOf[Byte])
      case 7 => regs.setChar(offset, value.asInstanceOf[Char])
      case 8 => regs.setShort(offset, value.asInstanceOf[Short])
      case 9 => ()
      case _ => regs.setObject(offset, value.asInstanceOf[AnyRef])

  private def load(regs: Registers, offset: RegisterOffset, tag: Int): Any =
    (tag: @switch) match
      case 1 => regs.getInt(offset)
      case 2 => regs.getLong(offset)
      case 3 => regs.getFloat(offset)
      case 4 => regs.getDouble(offset)
      case 5 => regs.getBoolean(offset)
      case 6 => regs.getByte(offset)
      case 7 => regs.getChar(offset)
      case 8 => regs.getShort(offset)
      case 9 => ()
      case _ => regs.getObject(offset)

  private enum FieldKind derives CanEqual:
    case Attribute(name: String)
    case Text
    case Child
    case Extras
    case Tag

  private final class FieldInfo(
    val fieldName: String,
    val kind: FieldKind,
    val optional: Boolean,
    val sequence: Boolean,
    val codec: XmlCodec[Any],
    val itemNames: Seq[String],
    val defaultValue: Option[Any],
    val offset: RegisterOffset,
    val typeTag: Int,
    val seqParts: Option[SeqParts]
  )

  private final class SeqParts(
    val fromItems: Seq[Any] => Any,
    val toItems: Any => Iterator[Any]
  )


