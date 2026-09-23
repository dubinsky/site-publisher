package org.podval.tools.publish.gradle;

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;
import org.gradle.testkit.runner.TaskOutcome;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class SitePublisherPluginTest {
  @TempDir
  Path projectDir;

  @Test
  void relativizeAndResolveTarget() {
    File source = new File("/tmp/site-src").getAbsoluteFile();
    File nested = new File(source, "_site");
    assertEquals("_site", SitePublisherPlugin.relativize(source, nested));
    assertEquals(nested.getAbsoluteFile(), SitePublisherPlugin.resolveTarget(source, "_site"));
    File abs = new File("/abs/out").getAbsoluteFile();
    assertEquals(abs, SitePublisherPlugin.resolveTarget(source, abs.getPath()));
    assertNull(SitePublisherPlugin.relativize(source, abs));
  }

  @Test
  void generateSiteAndServeSiteExistWithDetachedClasspath() throws IOException {
    writeProject(
      """
      plugins {
        id 'java'
        id 'org.podval.tools.site-publisher'
      }
      site {
        treatErrorsAsWarnings = true
        logLevel = 'INFO'
      }
      tasks.register('assertSitePublisher') {
        doLast {
          assert configurations.findByName('sitePublisher') != null
          assert !configurations.implementation.allDependencies.any { dep ->
            dep.group == 'org.podval.tools' && dep.name == 'org.podval.tools.publisher'
          }
          def gen = tasks.named('generateSite', JavaExec).get()
          def serve = tasks.named('serveSite', JavaExec).get()
          def pretty = tasks.named('prettyPrintSite', JavaExec).get()
          assert gen.mainClass.get() == 'org.podval.tools.publish.site.Site'
          assert serve.mainClass.get() == 'org.podval.tools.publish.site.Site'
          assert pretty.mainClass.get() == 'org.podval.tools.publish.site.Site'
          assert gen.outputs.files.files.any { it.name == '_site' }
          assert gen.inputs.hasInputs
          assert pretty.inputs.hasInputs
          def genArgs = gen.argumentProviders[0].asArguments().toList()
          def serveArgs = serve.argumentProviders[0].asArguments().toList()
          def prettyArgs = pretty.argumentProviders[0].asArguments().toList()
          assert genArgs.contains('--pretty-print=false')
          assert serveArgs.contains('--pretty-print=false')
          assert prettyArgs.contains('--pretty-print')
          assert !prettyArgs.any { it == '--serve' || it.startsWith('--serve=') }
          def buildTask = tasks.named('build').get()
          def buildDeps = buildTask.taskDependencies.getDependencies(buildTask)
          assert buildDeps.every {
            it.name != 'generateSite' && it.name != 'serveSite' && it.name != 'prettyPrintSite'
          }
        }
      }
      """
    );

    BuildResult result = runner("assertSitePublisher").build();
    assertNotNull(result.task(":assertSitePublisher"));
    assertEquals(TaskOutcome.SUCCESS, result.task(":assertSitePublisher").getOutcome());
  }

  @Test
  void customTargetDirectoryNameIsTheGenerateSiteOutput() throws IOException {
    writeProject(
      """
      plugins {
        id 'org.podval.tools.site-publisher'
      }
      site {
        targetDirectoryName = 'out-site'
      }
      tasks.register('assertTarget') {
        doLast {
          def gen = tasks.named('generateSite', JavaExec).get()
          assert gen.outputs.files.files.any { it.name == 'out-site' }
        }
      }
      """
    );

    BuildResult result = runner("assertTarget").build();
    assertNotNull(result.task(":assertTarget"));
    assertEquals(TaskOutcome.SUCCESS, result.task(":assertTarget").getOutcome());
  }

  @Test
  void configurationCacheOnHelp() throws IOException {
    writeProject(
      """
      plugins {
        id 'org.podval.tools.site-publisher'
      }
      """
    );

    BuildResult result = runner("help", "--configuration-cache").build();
    assertTrue(
      result.getOutput().contains("Configuration cache") || result.getOutput().contains("Reusing configuration cache"),
      result.getOutput()
    );
  }

  private void writeProject(String buildGradle) throws IOException {
    Files.writeString(
      projectDir.resolve("settings.gradle"),
      "rootProject.name = 'site-publisher-plugin-test'\n",
      StandardCharsets.UTF_8
    );
    Files.writeString(projectDir.resolve("build.gradle"), buildGradle, StandardCharsets.UTF_8);
    Files.writeString(
      projectDir.resolve("_site_config.yml"),
      "title: test\n",
      StandardCharsets.UTF_8
    );
  }

  private GradleRunner runner(String... arguments) {
    return GradleRunner.create()
      .withProjectDir(projectDir.toFile())
      .withPluginClasspath()
      .withArguments(arguments)
      .forwardOutput();
  }
}
