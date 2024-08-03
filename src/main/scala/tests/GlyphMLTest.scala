package org.sufrin.glyph
package tests

import markup._
import Glyphs.{lightGrey, nothing}
import GlyphTypes.Scalar
import Styles.GlyphStyle

class GlyphMLTest {

}

object GlyphMLTest extends Application {

  object LocalStyle extends Styles.DefaultSheet {
    override def buttonFontSize: Scalar = 32
    override def labelFontSize: Scalar = 24
    override def labelStyle: GlyphStyle = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }
  val Local: Context =
    Context(
      style        = LocalStyle,
      columnWidth  = 800f,
      leftMargin   = 0,
      rightMargin  = 0,
      parAlign     = Justify)
      .copy(fontFamily=Family("Courier"))
      .fontSize(28)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)

  val GUI: Glyph = new Resizeable(Local) {
    def element: Element = {
      Text("""GlyphML is a domain-specific language embedded in Scala: its elements denote Glyphs.
             |
             |The GlyphML API (which is still evolving) may eventually be slightly more convenient
             |than the standard Glyphs API for interface designers who don't need to get to grips with its
             |underview.
             |
             |""".stripMargin)(_.labelStyle(fg=DefaultBrushes.red)).framed(fg=DefaultBrushes.yellow strokeWidth 40f, bg=lightGrey)
    }
  }

  override def title: String = "Topdown Example"

  override val defaultIconPath: Option[String] = Some("./flag.png")
}
