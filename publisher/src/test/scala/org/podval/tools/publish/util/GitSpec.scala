package org.podval.tools.publish.util

import org.scalatest.funsuite.AnyFunSuite

final class GitSpec extends AnyFunSuite:
  test("github origin urls drop userinfo and .git") {
    assert(Git.githubWeb("git@github.com:opentorah/alter-rebbe.org.git")
      .contains("https://github.com/opentorah/alter-rebbe.org"))
    assert(Git.githubWeb("ssh://git@github.com/owner/repo.git")
      .contains("https://github.com/owner/repo"))
    assert(Git.githubWeb("https://github.com/owner/repo")
      .contains("https://github.com/owner/repo"))
    val token: String = "https://x-access-token:ghs_secret@github.com/owner/repo.git"
    val web: String = Git.githubWeb(token).get
    assert(web == "https://github.com/owner/repo")
    assert(!web.contains("ghs_secret"))
    assert(!web.contains("x-access-token"))
    assert(Git.githubWeb("https://gitlab.com/owner/repo.git").isEmpty)
    assert(Git.githubWeb("git@gitlab.com:owner/repo.git").isEmpty)
    assert(Git.githubWeb("").isEmpty)
  }

  test("repo path keeps the source directory under the work tree") {
    assert(Git.repoRelative("", "/index.md") == "index.md")
    assert(Git.repoRelative("docs", "/index.md") == "docs/index.md")
    assert(Git.repoRelative("publisher/src/test/site", "/notes.md") ==
      "publisher/src/test/site/notes.md")
  }

  test("branch is HEAD, else the Actions branch, else origin/HEAD") {
    assert(Git.branchName(Some("refs/heads/master"), None, None, Some("main")).contains("master"))
    assert(Git.branchName(
      fullBranch = Some("0123456789abcdef"),
      githubRef = Some("refs/heads/master"),
      githubRefName = Some("master"),
      originHead = Some("main")
    ).contains("master"))
    assert(Git.branchName(
      fullBranch = Some("0123456789abcdef"),
      githubRef = Some("refs/tags/v1"),
      githubRefName = Some("v1"),
      originHead = Some("master")
    ).contains("master"))
    assert(Git.branchName(None, None, None, None).isEmpty)
  }

  test("git path encoding keeps slashes") {
    assert(Git.encodeGitPath("archive/rgia/071.xml") == "archive/rgia/071.xml")
    assert(Git.encodeGitPath("name/Павел I.xml").contains("%20"))
    assert(!Git.encodeGitPath("feature/foo").contains("%2F"))
  }
