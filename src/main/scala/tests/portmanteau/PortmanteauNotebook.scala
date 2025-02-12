package org.sufrin.glyph
package tests.portmanteau

import GlyphTypes._

import org.sufrin.glyph.glyphXML.{Abstraction, Translation}
import org.sufrin.glyph.glyphXML.Translation.Target.{ColTarget, Target}
import org.sufrin.glyph.glyphXML.Visitor.AttributeMap
import org.sufrin.glyph.sheeted.BookSheet

import scala.xml.Node


object PortmanteauNotebook extends Application  {
  import Styles._

  /**
   * Default sheet
   */
  val LocalSheet: Sheet = Sheet()

  val interfaceStyle: Sheet = LocalSheet.copy(
    buttonFrame=Styles.Decoration.Blurred(fg=DefaultBrushes.blue, blur=5, spread=5, delta=5),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = DefaultBrushes.white
  )
  implicit val bookStyle: BookSheet =
    BookSheet(buttonSheet=interfaceStyle,
              pageSheet=interfaceStyle.copy(buttonFrame=Decoration.Unframed, fontScale=0.9f))

  import glyphXML.Language._

  val interface = new Interface

  lazy val GUI: Glyph = interface.asRNotebook

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = interface.confirmCloseOn(GUI)(window)
}
