package org.podval.tools.publish.util

import scala.jdk.CollectionConverters.IterableHasAsScala
import org.eclipse.jgit.api.Git as JGit
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.File
import java.time.Instant

final class Git(rootDirectory: File):
  private lazy val repository: Option[Repository] =
    val dotGit = File(rootDirectory, ".git")
    Option.when(dotGit.exists)(FileRepositoryBuilder().setGitDir(dotGit).build)

  private lazy val git: Option[JGit] = repository.map(JGit(_))

  // Equivalent to running: git log -1 -- path/to/file
  def modificationDate(filePath: String): Option[Instant] = git.flatMap(_
      .log
      .addPath(filePath.substring(1)) // chop off leading slash
      .setMaxCount(1)
      .call
      .asScala
      .headOption
      .map(_.getCommitTime*1L)
      .map(Instant.ofEpochSecond)
  )



