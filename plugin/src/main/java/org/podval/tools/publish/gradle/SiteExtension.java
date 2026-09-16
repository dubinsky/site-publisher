package org.podval.tools.publish.gradle;

import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;

/** Consumer-facing {@code site { }} block. Defaults match {@code SiteOptions}. */
public abstract class SiteExtension {
  /** Directory that contains {@code _site_config.yml}. Default: the Gradle project directory. */
  public abstract DirectoryProperty getSourceDirectory();

  /** {@code --target-directory-name}. Default: {@code _site}. Absolute paths are used as-is. */
  public abstract Property<String> getTargetDirectoryName();

  /** {@code --treat-errors-as-warnings}. Default: {@code false}. */
  public abstract Property<Boolean> getTreatErrorsAsWarnings();

  /** {@code --log-level}. Default: {@code INFO}. */
  public abstract Property<String> getLogLevel();

  /** {@code --include-drafts}. Default: {@code false}. */
  public abstract Property<Boolean> getIncludeDrafts();

  /** {@code --production}. Default: {@code false}. */
  public abstract Property<Boolean> getProduction();
}
