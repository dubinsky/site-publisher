package org.podval.tools.publish.gradle;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.Properties;

/** Version of {@code org.podval.tools:org.podval.tools.publisher} this plugin depends on (same as the plugin). */
final class PublisherVersion {
  private PublisherVersion() {}

  static String get() {
    try (InputStream in = PublisherVersion.class.getResourceAsStream("version.properties")) {
      if (in == null) {
        throw new IllegalStateException("Missing org/podval/tools/publish/gradle/version.properties");
      }
      Properties properties = new Properties();
      properties.load(in);
      String version = properties.getProperty("version");
      if (version == null || version.isBlank()) {
        throw new IllegalStateException("version.properties has no version=");
      }
      return version;
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
