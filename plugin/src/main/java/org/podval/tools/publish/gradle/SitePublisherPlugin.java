package org.podval.tools.publish.gradle;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.file.ConfigurableFileTree;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Provider;
import org.gradle.api.tasks.JavaExec;
import org.gradle.api.tasks.PathSensitivity;
import org.gradle.jvm.toolchain.JavaLanguageVersion;
import org.gradle.jvm.toolchain.JavaLauncher;
import org.gradle.jvm.toolchain.JavaToolchainService;
import java.io.File;
import java.nio.file.Path;
import java.util.List;

/**
 * Registers {@code generateSite} / {@code serveSite} as {@link JavaExec} on a detached
 * {@code sitePublisher} configuration. Does not put the publisher on the consumer compile classpath.
 */
public final class SitePublisherPlugin implements Plugin<Project> {
  public static final String EXTENSION_NAME = "site";
  public static final String CONFIGURATION_NAME = "sitePublisher";
  public static final String GENERATE_TASK = "generateSite";
  public static final String SERVE_TASK = "serveSite";
  public static final String MAIN_CLASS = "org.podval.tools.publish.site.Site";
  public static final String PUBLISHER_COORDINATE =
    "org.podval.tools:org.podval.tools.publisher:" + PublisherVersion.get();

  private static final List<String> JDK_COMPAT_JVM_ARGS = List.of(
    "--enable-native-access=ALL-UNNAMED",
    "--sun-misc-unsafe-memory-access=allow"
  );

  @Override
  public void apply(Project project) {
    project.getPluginManager().apply("jvm-toolchains");

    SiteExtension extension = project.getExtensions().create(EXTENSION_NAME, SiteExtension.class);
    extension.getSourceDirectory().convention(project.getLayout().getProjectDirectory());
    extension.getTargetDirectoryName().convention("_site");
    extension.getTreatErrorsAsWarnings().convention(false);
    extension.getLogLevel().convention("INFO");
    extension.getIncludeDrafts().convention(false);
    extension.getProduction().convention(false);

    Configuration classpath = project.getConfigurations().create(CONFIGURATION_NAME, configuration -> {
      configuration.setDescription(
        "Classpath for generateSite / serveSite (org.podval.tools.publisher). Not the compile classpath."
      );
      configuration.setCanBeConsumed(false);
      configuration.setCanBeResolved(true);
      configuration.defaultDependencies(dependencies ->
        dependencies.add(project.getDependencies().create(PUBLISHER_COORDINATE))
      );
    });

    JavaToolchainService toolchains = project.getExtensions().getByType(JavaToolchainService.class);
    Provider<JavaLauncher> java25 = toolchains.launcherFor(spec ->
      spec.getLanguageVersion().set(JavaLanguageVersion.of(25))
    );

    registerSiteExec(project, extension, classpath, java25, GENERATE_TASK,
      "Generate the static site into the target directory", false);
    registerSiteExec(project, extension, classpath, java25, SERVE_TASK,
      "Generate the static site and serve it locally", true);
  }

  private static void registerSiteExec(
    Project project,
    SiteExtension extension,
    Configuration classpath,
    Provider<JavaLauncher> javaLauncher,
    String taskName,
    String description,
    boolean serve
  ) {
    project.getTasks().register(taskName, JavaExec.class, task -> {
      task.setGroup("documentation");
      task.setDescription(description);
      task.getMainClass().set(MAIN_CLASS);
      task.classpath(classpath);
      task.getJavaLauncher().set(javaLauncher);
      task.jvmArgs(JDK_COMPAT_JVM_ARGS);
      task.environment("PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS", "1");
      task.environment(
        "PLAYWRIGHT_BROWSERS_PATH",
        new File(project.getGradle().getGradleUserHomeDir(), "ms-playwright").getAbsolutePath()
      );

      SiteArgsProvider args = project.getObjects().newInstance(SiteArgsProvider.class);
      args.getSourcePath().set(extension.getSourceDirectory().map(dir -> dir.getAsFile().getAbsolutePath()));
      args.getTargetDirectoryName().set(extension.getTargetDirectoryName());
      args.getTreatErrorsAsWarnings().set(extension.getTreatErrorsAsWarnings());
      args.getLogLevel().set(extension.getLogLevel());
      args.getIncludeDrafts().set(extension.getIncludeDrafts());
      args.getProduction().set(extension.getProduction());
      args.getServe().set(serve);
      task.getArgumentProviders().add(args);

      DirectoryProperty sourceDirectory = extension.getSourceDirectory();
      Provider<String> targetDirectoryName = extension.getTargetDirectoryName();
      Provider<File> targetDirectory = project.provider(() ->
        resolveTarget(sourceDirectory.get().getAsFile(), targetDirectoryName.get())
      );

      task.getInputs().files(project.provider(() ->
        sourceTree(project, sourceDirectory.get().getAsFile(), targetDirectory.get())
      )).withPropertyName("siteSources")
        .withPathSensitivity(PathSensitivity.RELATIVE)
        .ignoreEmptyDirectories();

      task.getOutputs().dir(project.getLayout().dir(targetDirectory)).withPropertyName("siteTarget");
      // The generator rewrites the tree when it runs; up-to-date can skip the run. Do not cache.
      task.getOutputs().cacheIf(ignored -> false);

      if (serve) {
        task.getOutputs().upToDateWhen(ignored -> false);
      }
    });
  }

  static File resolveTarget(File sourceDirectory, String targetDirectoryName) {
    File named = new File(targetDirectoryName);
    File target = named.isAbsolute() ? named : new File(sourceDirectory, targetDirectoryName);
    return target.getAbsoluteFile();
  }

  static ConfigurableFileTree sourceTree(Project project, File sourceDirectory, File targetDirectory) {
    ConfigurableFileTree tree = project.fileTree(sourceDirectory);
    tree.exclude("build/**");
    tree.exclude(".gradle/**");
    String relativeTarget = relativize(sourceDirectory.getAbsoluteFile(), targetDirectory);
    if (relativeTarget != null) {
      tree.exclude(relativeTarget);
      tree.exclude(relativeTarget + "/**");
    }
    return tree;
  }

  /** Relative path of {@code child} under {@code parent}, or {@code null} if it is not a descendant. */
  static String relativize(File parent, File child) {
    Path parentPath = parent.toPath().toAbsolutePath().normalize();
    Path childPath = child.toPath().toAbsolutePath().normalize();
    if (!childPath.startsWith(parentPath)) return null;
    String relative = parentPath.relativize(childPath).toString().replace('\\', '/');
    return relative.isEmpty() ? null : relative;
  }
}
