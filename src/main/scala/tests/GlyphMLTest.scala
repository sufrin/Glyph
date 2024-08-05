package org.sufrin.glyph
package tests

import markup._
import Glyphs._
import GlyphTypes.Scalar
import Styles.{Decoration, GlyphStyle}

import org.sufrin.logging.FINE

class GlyphMLTest {

}

object GlyphMLTest extends Application {

  object LocalStyle extends Styles.DefaultSheet {
    override def buttonFontSize: Scalar = 22
    override def labelFontSize: Scalar = 22
    override def labelStyle: GlyphStyle = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }
  val Local: Context =
    Context(
      style        = LocalStyle,
      columnWidth  = 400f,
      boundingBox  = Vec(400, 250),
      leftMargin   = 0,
      rightMargin  = 0,
      parAlign     = Justify)
      .copy(fontFamily=Family("Arial"))
      .fontSize(22)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)
      .frameStyle(Decoration.Blurred(fg=blue, blur=15f, spread=15f), fg=white)
      //.frameStyle(decoration=Decoration.Shaded(fg=blue, bg=darkGrey))

  val t1 = Text("""GlyphML is a domain-specific language embedded in Scala: its elements denote Glyphs.
                  |
                  |The GlyphML API (which is still evolving) may eventually be slightly more convenient
                  |than the standard Glyphs API for interface designers who don't need to get to grips with its
                  |underview.
                  |
                  |""".stripMargin)(_.labelStyle(fg=DefaultBrushes.red)).framed(fg=DefaultBrushes.black strokeWidth 0, bg=lightGrey)

  val t2 = Text("""This is an experiment in choosing the details of GUI layout dynamically.
                  |
                  |Initially these two paragraphs appear beside each other in their "natural" sizes
                  |for the current context. But if the window is made long enough, they may
                  |appear as a column.
                  |
                  |""".stripMargin)(_.labelStyle(fg=DefaultBrushes.red)).framed(fg=DefaultBrushes.black strokeWidth 0, bg=lightGrey)

  val anchor = Cached(Local)(INVISIBLE())
  val r1 = Cached(Local)(t1)
  val r2 = Cached(Local)(t2)
  val traceOn: Glyph = styled.TextButton("Trace on") {
    _ =>
      val root = anchor.root
      root.decodeMotionModifiers(true)
      root.trackMouseState(true)
      RootGlyph.level=FINE
  }(Local.style)
  val traceOff: Glyph = styled.TextButton("Trace off") {
    _ =>
      val root = anchor.root
      root.decodeMotionModifiers(false)
      root.trackMouseState(false)
      RootGlyph.level=FINE
  }(Local.style)


  val GUI: Glyph = Resizeable(Local) {
    def menu =  MenuBar(Local)(traceOn, Gap, traceOff)
    def splitRow(h: Scalar) = {
      val tw = r1.w+r2.w
      Column(menu, Row(r1.scaled(r1.w/tw, 1), Glyphs.FilledRect(6, h, darkGrey), r2.scaled(r2.w/tw, 1)))
    }
    def splitCol(w: Scalar) = {
      val th = r1.h+r2.h
      Column(Column(r1.scaled(1, r1.h/th), menu, r2.scaled(1, r2.h/th)))
    }
    /** An experiment in dynamic layout  */
    Dynamic {
      case Vec(0, 0)        => splitCol(50f)
      case Vec(w, h) if w>h => splitRow(h)
      case Vec(w, h)        => splitCol(w)
      }
  }

  override def title: String = "Topdown Example"

  override val defaultIconPath: Option[String] = Some("./flag.png")
}
