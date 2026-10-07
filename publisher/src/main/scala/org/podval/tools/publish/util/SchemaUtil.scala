package org.podval.tools.publish.util

import zio.blocks.schema.binding.Binding
import zio.blocks.schema.yaml.Yaml
import zio.blocks.schema.{NameMapper, Reflect, Schema}

object SchemaUtil:
  def fieldNames[A](schema: Schema[A]): Set[String] = schema
    .reflect
    .asRecord
    .get
    .fields
    .map(_.name)
    .toSet

  /**
   * Keys the YAML codec will not bind.
   * Record field names are kebab-case, the same spelling `YamlCodecDeriver` hardcodes.
   * A nested record (`social`, `graph`) is walked.
   * A value with the wrong shape is left to the codec.
   */
  def unknownKeys(yaml: Yaml, schema: Schema[?]): List[String] =
    unknown(yaml, schema.reflect, Nil).distinct

  private def unknown(yaml: Yaml, reflect: Reflect.Bound[?], prefix: List[String]): List[String] =
    asRecord(reflect) match
      case None => Nil
      case Some(record) => yaml match
        case Yaml.Mapping(entries) =>
          val fields: Map[String, Reflect.Bound[?]] = record.fields
            .map(field => NameMapper.KebabCase(field.name) -> field.value)
            .toMap
          entries.toList.flatMap: (key, value) =>
            key match
              case Yaml.Scalar(name, _) =>
                fields.get(name) match
                  case Some(field) => unknown(value, field, prefix.appended(name))
                  case None => List(join(prefix, name))
              case _ => List(join(prefix, "(non-scalar key)"))
        case _ => Nil

  private def asRecord(reflect: Reflect.Bound[?]): Option[Reflect.Record[Binding, ?]] =
    val target: Reflect.Bound[?] = if reflect.isOption then reflect.optionInnerType.getOrElse(reflect) else reflect
    target.asRecord

  private def join(prefix: List[String], name: String): String =
    if prefix.isEmpty then name else s"${prefix.mkString(".")}.$name"
