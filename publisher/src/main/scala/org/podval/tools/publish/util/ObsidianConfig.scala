package org.podval.tools.publish.util

import org.podval.tools.publish.util.Files
import zio.blocks.schema.json.{Json, JsonType}
import java.io.File

final class ObsidianConfig(directory: File):
  private def obsidianDirectory: File = File(directory, ".obsidian")
  private def dailyNotesFile: File = File(obsidianDirectory, "daily-notes.json")

  lazy val daysFolder: Option[String] =
    if !dailyNotesFile.exists then None else
      Json.parse(Files.read(dailyNotesFile)) match
        case Left(error) => None
        case Right(json) => json.get("folder").one match
          case Left(error) => None
          case Right(value) => value.unwrap(JsonType.String)
