package org.sufrin.glyph
package files

import java.nio.file.{FileStore, FileSystems, Path}
import scala.jdk.CollectionConverters.IterableHasAsScala

object System {
  lazy val fileStores: Iterable[FileStore] =
    FileSystems.getDefault.getFileStores.asScala
  lazy val rootDirs: Iterable[Path] =
    FileSystems.getDefault.getRootDirectories.asScala
  lazy val supported: Set[String] =
    FileSystems.getDefault.supportedFileAttributeViews.asScala.toSet
}
