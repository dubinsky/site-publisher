package org.podval.tools.publish.prose

import org.scalatest.funsuite.AnyFunSuite
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

final class ProsePrettyPrintSpec extends AnyFunSuite:
  private def withDir(body: Path => Unit): Unit =
    val dir: Path = Files.createTempDirectory("prose-pretty")
    try body(dir)
    finally delete(dir)

  private def delete(dir: Path): Unit =
    if Files.exists(dir) then
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)

  test("an unchanged file is not replaced") {
    withDir: dir =>
      val file: Path = dir.resolve("a.md")
      Files.writeString(file, "Hello.\n")
      val mtime = Files.getLastModifiedTime(file)
      assert(ProsePrettyPrint.run(Array(file.toString)) == 0)
      assert(Files.getLastModifiedTime(file).toMillis == mtime.toMillis)
      assert(!Files.exists(dir.resolve("a.md.pretty-print.tmp")))
  }

  test("bad utf-8 and crlf are not written") {
    withDir: dir =>
      val bad: Path = dir.resolve("bad.md")
      Files.write(bad, Array[Byte](0xff.toByte))
      assert(ProsePrettyPrint.run(Array(bad.toString)) == 1)
      assert(Files.readAllBytes(bad).sameElements(Array[Byte](0xff.toByte)))
      val crlf: Path = dir.resolve("cr.md")
      Files.write(crlf, "Hello.\r\n".getBytes(StandardCharsets.UTF_8))
      assert(ProsePrettyPrint.run(Array(crlf.toString)) == 1)
      assert(Files.readString(crlf) == "Hello.\r\n")
  }

  test("a memory path is refused") {
    withDir: dir =>
      val home: Path = dir.resolve("home")
      val file: Path = home.resolve(".grok").resolve("memory-v2").resolve("note.md")
      Files.createDirectories(file.getParent)
      Files.writeString(file, "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array(file.toString), home) == 1)
      assert(Files.readString(file) == "Hello world. Next.\n")
  }

  test("a directory honors _site_ignore and a malformed file fails") {
    withDir: dir =>
      Files.writeString(dir.resolve("_site_ignore"), "skip.md\n")
      Files.writeString(dir.resolve("skip.md"), "Hello world. Next.\n")
      Files.writeString(dir.resolve("keep.md"), "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array(dir.toString)) == 0)
      assert(Files.readString(dir.resolve("skip.md")) == "Hello world. Next.\n")
      assert(Files.readString(dir.resolve("keep.md")).contains("Hello world.\nNext."))
      Files.writeString(dir.resolve("open.md"), "+++\ntitle\nHello.\n")
      assert(ProsePrettyPrint.run(Array(dir.toString)) == 1)
  }

  test("a named text file and an asset sidecar fail") {
    withDir: dir =>
      val text: Path = dir.resolve("a.txt")
      Files.writeString(text, "Hello.\n")
      assert(ProsePrettyPrint.run(Array(text.toString)) == 1)
      val note: Path = dir.resolve("note.md")
      Files.writeString(note, "Hello world. Next.\n")
      Files.writeString(dir.resolve("note.yml"), "asset: true\n")
      assert(ProsePrettyPrint.run(Array(note.toString)) == 1)
      assert(Files.readString(note) == "Hello world. Next.\n")
  }

  test("width flags") {
    withDir: dir =>
      val file: Path = dir.resolve("a.md")
      Files.writeString(file, "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array("--width=0", file.toString)) == 0)
      assert(Files.readString(file) == "Hello world.\nNext.\n")
      Files.writeString(file, "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array("--sentence-per-line=false", file.toString)) == 0)
      assert(Files.readString(file) == "Hello world. Next.\n")
      Files.writeString(file, "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array("--width", file.toString)) == 2)
      assert(Files.readString(file) == "Hello world. Next.\n")
      assert(ProsePrettyPrint.run(Array("--width=-1", file.toString)) == 2)
      assert(Files.readString(file) == "Hello world. Next.\n")
  }
