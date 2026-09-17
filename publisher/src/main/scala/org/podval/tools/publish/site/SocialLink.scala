package org.podval.tools.publish.site

sealed abstract class SocialLink(
  val title: String,
  val site: String,
  val icon: String,
  val userName: String
):
  final def href: String = s"https://$site/$userName"

object SocialLink:
  final class GitHub(userName: String) extends SocialLink(
    title = "GitHub",
    site = "github.com",
    icon = "github",
    userName = userName
  )

  final class Twitter(userName: String) extends SocialLink(
    title = "Twitter",
    site = "www.twitter.com",
    icon = "twitter",
    userName = userName
  )

  final class LinkedIn(userName: String) extends SocialLink(
    title = "LinkedIn",
    site = "www.linkedin.com/in",
    icon = "linkedin",
    userName = userName
  )
