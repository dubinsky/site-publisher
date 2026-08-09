package org.podval.tools.publish.util

import com.microsoft.playwright.{Browser, BrowserType, Playwright}

// Arch (and other non-Ubuntu hosts) often trip Playwright's Debian-oriented
// dependency check even when headless Chromium works. Skip the check for the driver.
object PlaywrightUtil:
  def playwright: Playwright = Playwright
    .create(
      Playwright.CreateOptions().setEnv(java.util.Map.of(
        "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS", "1"
      ))
    )

  def browser(playwright: Playwright): Browser =
    playwright.chromium.launch(
      BrowserType.LaunchOptions().setHeadless(true)
    )
