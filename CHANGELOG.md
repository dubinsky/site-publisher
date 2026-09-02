# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
- xml: Gradle subproject `org.podval.xml` with a Schema-derived document binder (`XmlCodec`) over any `XmlAst` (unwrapped sequences, leaf-record attributes, `XmlNode` identity, leftover `XmlExtras`)
- xml: removed `RawXml` / `WithRawXml`; `Entity` / `EntityReference` keep leftovers in `XmlExtras`
- xml: `XmlCodec` lives in `org.podval.xml`; Selector, collection parts, and entity-lists decode with it

## [0.0.1] - 2026-
- chore: initial check-in
