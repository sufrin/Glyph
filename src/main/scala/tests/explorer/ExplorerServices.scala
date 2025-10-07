package org.sufrin.glyph
package tests.explorer

import java.nio.file._

trait ExplorerServices {
  /** open the object denoted by `path` */
  def openPath(path: Path): Unit
  /** close the `Explorer` opened for `path` */
  def closeExplorer(path: Path): Unit
  /** Open a new `ExplorerServices` initially showing `path` */
  def openServices(path: Path): Unit
}











