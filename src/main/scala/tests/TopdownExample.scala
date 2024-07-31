package org.sufrin.glyph
package tests

import markup._
import Glyphs.nothing
import GlyphTypes.Scalar
import Styles.GlyphStyle

class TopdownExample {

}

object TopdownExample extends Application {

  object LocalStyle extends Styles.DefaultSheet {
    override def buttonFontSize: Scalar = 32
    override def labelFontSize: Scalar = 24
    override def labelStyle: GlyphStyle = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }
  val LocalContext: Context =
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

  val GUI: Glyph = new PortmanteauGlyphML()(LocalStyle) {
    def element: Element = {
      Text("""GlyphML is a domain-specific language embedded in Scala: its expressions denote Glyphs.
             |
             |The GlyphML API (which is still evolving rapidly) may eventually be slightly more convenient
             |than the standard Glyphs API for interface designers who don't need to get to grips with its
             |underview.
             |
             |There are a couple of important principles to get to grips with:
             |""".stripMargin)
    }
  }.GUI

  override def title: String = "Topdown Example"

  override val defaultIconPath: Option[String] = Some("./flag.png")
}
