package org.sufrin.glyph
package tests.barents

import java.nio.file._

trait ViewerServices {
  /** open the object denoted by `path` */
  def openPath(path: Path): Unit
  /** close the `Viewer` opened for `path` */
  def closeExplorer(path: Path): Unit
  /** Open a new `ViewerServices` initially showing `path` */
  def openServices(path: Path): Unit
}











