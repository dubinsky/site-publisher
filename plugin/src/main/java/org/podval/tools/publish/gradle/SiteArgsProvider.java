package org.podval.tools.publish.gradle;

import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.process.CommandLineArgumentProvider;
import java.util.ArrayList;
import java.util.List;

/** CLI args for {@code Site.main}. Gradle-managed properties so configuration cache can serialize them. */
public abstract class SiteArgsProvider implements CommandLineArgumentProvider {
  @Input public abstract Property<String> getSourcePath();
  @Input public abstract Property<String> getTargetDirectoryName();
  @Input public abstract Property<Boolean> getTreatErrorsAsWarnings();
  @Input public abstract Property<String> getLogLevel();
  @Input public abstract Property<Boolean> getIncludeDrafts();
  @Input public abstract Property<Boolean> getProduction();
  @Input public abstract Property<Boolean> getServe();

  @Override
  public Iterable<String> asArguments() {
    List<String> args = new ArrayList<>();
    args.add(getSourcePath().get());
    args.add("--target-directory-name=" + getTargetDirectoryName().get());
    args.add("--log-level=" + getLogLevel().get());
    if (getTreatErrorsAsWarnings().get()) args.add("--treat-errors-as-warnings");
    if (getIncludeDrafts().get()) args.add("--include-drafts");
    if (getProduction().get()) args.add("--production");
    if (getServe().get()) args.add("--serve");
    return args;
  }
}
