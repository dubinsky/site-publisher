package org.podval.tools.publish.util

import zio.blocks.schema.Schema

object SchemaUtil:
  def fieldNames[A](schema: Schema[A]): Set[String] = schema
    .reflect
    .asRecord
    .get
    .fields
    .map(_.name)
    .toSet
  