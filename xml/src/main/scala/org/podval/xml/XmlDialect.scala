package org.podval.xml

// TODO use or discard

//  val header: String = """<?xml version="1.0" encoding="UTF-8"?>"""
//  val header16: String = """<?xml version="1.0" encoding="UTF-16"?>"""

trait XmlDialect:
//  def namespace: Namespace

  def mimeType: String

  def rootElementName: String

  def dtdId: Option[String] = None

  def dtdUri: Option[String] = None

  final def doctype: String = doctype(rootElementName)

  final def doctype(rootElementName: String): String =
    val ids: String = dtdId.fold("")(it => s"""" $it"""") + dtdUri.fold("")(it => s"""" $it"""")
    val inner: String = if ids.isEmpty then "" else s" PUBLIC$ids"
    s"<!DOCTYPE $rootElementName$inner>"
