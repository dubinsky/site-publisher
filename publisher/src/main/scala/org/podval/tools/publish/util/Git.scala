package org.podval.tools.publish.util

import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.control.NonFatal
import org.eclipse.jgit.api.Git as JGit
import org.eclipse.jgit.lib.{Constants, Repository, SymbolicRef}
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import java.io.File
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets
import java.time.Instant

/** Git metadata for the site source directory.
  *
  * The source directory may sit below the work tree (`docs/` has no `.git`).
  * `findGitDir` walks up and also accepts a worktree `.git` file.
  */
final class Git(sourceDirectory: File):
  private lazy val opened: Option[Git.Opened] = Git.open(sourceDirectory)

  // Equivalent to running: git log -1 -- path/to/file
  // `filePath` is the site source path (`/index.md`). The log path is relative to the work tree.
  def modificationDate(filePath: String): Option[Instant] = opened.flatMap: opened =>
    val relative: String = Git.repoRelative(opened.prefix, filePath)
    Option.when(relative.nonEmpty)(relative).flatMap: path =>
      opened.git.log
        .addPath(path)
        .setMaxCount(1)
        .call
        .asScala
        .headOption
        .map(commit => Instant.ofEpochSecond(commit.getCommitTime.toLong))

  /** `https://github.com/owner/name/blob/<branch>/<path>`, or none when origin is not GitHub. */
  def blobUrl(sourcePath: String): Option[String] = opened.flatMap: opened =>
    val relative: String = Git.repoRelative(opened.prefix, sourcePath)
    for
      web <- Git.githubWeb(Git.originUrl(opened.repo))
      branch <- Git.branchOf(opened.repo)
      if relative.nonEmpty
    yield s"$web/blob/${Git.encodeGitPath(branch)}/${Git.encodeGitPath(relative)}"

  /** HEAD of this build: the commit the citation quotes. None when there is no repository. */
  def head: Option[Git.Head] = opened.flatMap(opened => Git.headCommit(opened.repo))

object Git:
  final class Opened(val repo: Repository, val prefix: String):
    val git: JGit = JGit(repo)

  final case class Head(sha: String, time: Instant)

  /** Work-tree path of a site source path. `prefix` is the source directory relative to the work tree. */
  def repoRelative(prefix: String, sourcePath: String): String =
    val chopped: String = sourcePath.dropWhile(_ == '/')
    if prefix.isEmpty then chopped
    else if chopped.isEmpty then prefix
    else s"$prefix/$chopped"

  /** `git@github.com:owner/repo.git`, `ssh://git@github.com/owner/repo.git`, and
    * `https://user:token@github.com/owner/repo.git` become `https://github.com/owner/repo`.
    * Userinfo is dropped. A non-GitHub host is none.
    */
  def githubWeb(remote: String): Option[String] =
    val trimmed: String = remote.trim
    if trimmed.isEmpty then None
    else if trimmed.contains("://") then webFromUri(trimmed)
    else webFromScp(trimmed)

  /** Branch of HEAD, else `GITHUB_REF_NAME` when Actions has detached `refs/heads/…`, else `origin/HEAD`. */
  def branchName(
    fullBranch: Option[String],
    githubRef: Option[String],
    githubRefName: Option[String],
    originHead: Option[String]
  ): Option[String] =
    fullBranch.flatMap(localBranch)
      .orElse(actionsBranch(githubRef, githubRefName))
      .orElse(originHead.map(_.trim).filter(_.nonEmpty))

  /** Encode each segment. Slashes stay, so a branch name `feature/foo` is still two segments. */
  def encodeGitPath(path: String): String =
    path.split("/", -1).map(encodeSegment).mkString("/")

  private def encodeSegment(segment: String): String =
    URLEncoder.encode(segment, StandardCharsets.UTF_8).replace("+", "%20")

  private def localBranch(full: String): Option[String] =
    Option.when(full.startsWith(Constants.R_HEADS))(full.drop(Constants.R_HEADS.length)).filter(_.nonEmpty)

  private def actionsBranch(githubRef: Option[String], githubRefName: Option[String]): Option[String] =
    val ref: Option[String] = githubRef.map(_.trim).filter(_.nonEmpty)
    val name: Option[String] = githubRefName.map(_.trim).filter(_.nonEmpty)
    ref.filter(_.startsWith(Constants.R_HEADS)).flatMap(_ => name)

  private def webFromScp(remote: String): Option[String] =
    remote.span(_ != '@') match
      case (user, rest) if user.nonEmpty && rest.startsWith("@") =>
        rest.drop(1).span(_ != ':') match
          case (host, path) if host.nonEmpty && path.startsWith(":") =>
            repository(host, path.drop(1))
          case _ => None
      case _ => None

  private def webFromUri(remote: String): Option[String] =
    try
      val uri: URI = URI(remote)
      val host: String = Option(uri.getHost).getOrElse("")
      val path: String = Option(uri.getPath).getOrElse("").dropWhile(_ == '/')
      repository(host, path)
    catch case NonFatal(_) => None

  private def repository(host: String, path: String): Option[String] =
    val normalized: String = host.trim.toLowerCase
    val github: Boolean = normalized == "github.com" || normalized == "www.github.com"
    if !github then None
    else
      val cleaned: String = path.trim.dropWhile(_ == '/').stripSuffix("/").stripSuffix(".git")
      cleaned.split("/").toSeq.filter(_.nonEmpty) match
        case owner +: name +: _ => Some(s"https://github.com/$owner/$name")
        case _ => None

  private[util] def open(directory: File): Option[Opened] =
    try
      val builder: FileRepositoryBuilder = FileRepositoryBuilder()
      builder.findGitDir(directory.getCanonicalFile)
      val gitDir: File = builder.getGitDir
      if gitDir == null then None
      else
        val repo: Repository = builder.setMustExist(true).build()
        val work: File = Option(repo.getWorkTree).getOrElse(gitDir.getParentFile)
        Some(Opened(repo, prefixOf(work, directory)))
    catch case NonFatal(_) => None

  private def prefixOf(workTree: File, sourceDirectory: File): String =
    val relative: String = workTree.getCanonicalFile.toPath
      .relativize(sourceDirectory.getCanonicalFile.toPath)
      .toString
      .replace('\\', '/')
    if relative.isEmpty || relative == "." || relative.startsWith("..") then "" else relative

  private def originUrl(repo: Repository): String =
    Option(repo.getConfig.getString("remote", "origin", "url")).map(_.trim).getOrElse("")

  private def branchOf(repo: Repository): Option[String] =
    try
      val full: Option[String] = Option(repo.getFullBranch).map(_.trim).filter(_.nonEmpty)
      branchName(
        fullBranch = full,
        githubRef = sys.env.get("GITHUB_REF"),
        githubRefName = sys.env.get("GITHUB_REF_NAME"),
        originHead = originDefaultBranch(repo)
      )
    catch case NonFatal(_) => None

  private def originDefaultBranch(repo: Repository): Option[String] =
    repo.exactRef(Constants.R_REMOTES + "origin/HEAD") match
      case sym: SymbolicRef =>
        val target: String = Option(sym.getTarget).map(_.getName).getOrElse("")
        val prefix: String = Constants.R_REMOTES + "origin/"
        Option.when(target.startsWith(prefix))(target.drop(prefix.length)).filter(_.nonEmpty)
      case _ => None

  private def headCommit(repo: Repository): Option[Head] =
    try
      val id = repo.resolve(Constants.HEAD)
      if id == null then None
      else
        val walk: RevWalk = RevWalk(repo)
        try
          val commit = walk.parseCommit(id)
          Some(Head(commit.getName, Instant.ofEpochSecond(commit.getCommitTime.toLong)))
        finally
          walk.close()
    catch case NonFatal(_) => None
