package org.sufrin.glyph
package tests

import GlyphTypes._

import org.sufrin.glyph.glyphXML.{Abstraction, Translation}
import org.sufrin.glyph.glyphXML.Translation.Target.{ColTarget, Target}
import org.sufrin.glyph.glyphXML.Visitor.AttributeMap

import scala.xml.Node


object PortmanteauNotebook extends Application  {
  import Styles._

  /**
   * Default sheet
   */
  val LocalSheet: Sheet = Sheet()

  implicit val interfaceStyle: Sheet = LocalSheet.copy(
    buttonFrame=Styles.Decoration.Blurred(fg=DefaultBrushes.blue, blur=5, spread=5, delta=5),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = DefaultBrushes.white
  )

  import glyphXML.Language._

  val interface = new PortmanteauInterface()

  lazy val GUI: Glyph = interface.asRNotebook

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = interface.confirmCloseOn(GUI)(window)
}
