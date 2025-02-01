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
    buttonFontSize = 28,
    labelFontSize = 28,
    textFontSize = 28
  )

  implicit val translator: Translation = new Translation()

  // Extend the basic XML semantics
  locally {
    translator("body") = new Translation(translator.primitives) {
      override def toString: String = "body translation"
      override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
        val child$ = child.filterNot(Translation.isBlank(_))
        List(ColTarget(sheet.backgroundBrush, chunks=super.translate(tags, false, attributes, sheet, child$)))
      }
    }

    translator("label") = new Abstraction(<span textFontFamily="Courier"><n>&BODY;</n></span>)

    def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(translator.primitives) {
      override def toString: String = s"StyleTranslation($tag, $textStyle)"
      override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
        super.translate(tag::tags, paragraph, attributes.updated("textStyle", textStyle), sheet, child)
      }
    }

    translator("i")  = textStyleTranslation("i",  "Italic")
    translator("b")  = textStyleTranslation("b",  "Bold")
    translator("bi") = textStyleTranslation("bi", "BoldItalic")
    translator("n")  = textStyleTranslation("n",  "Normal")
  }

  val interface = new PortmanteauInterface()

  lazy val GUI: Glyph = interface.asRNotebook

  def title = s"""PortmanteauNotebook -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

  override
  val defaultIconPath: Option[String] = Some ("./flag.png")

  override
  def onClose(window: Window): Unit = interface.confirmCloseOn(GUI)(window)
}
