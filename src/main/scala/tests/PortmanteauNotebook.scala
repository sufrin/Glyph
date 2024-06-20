package org.sufrin.glyph
package tests

import GlyphTypes._


object PortmanteauNotebook extends Application with PortmanteauInterface {

  lazy val GUI: Glyph = asRNotebook

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = confirmCloseOn(GUI)(window)
}
