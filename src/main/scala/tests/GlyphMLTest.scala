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
      paragraphWidth  = 400,
      boundingBox  = Vec(400, 250),
      leftMargin   = 0,
      rightMargin  = 0,
      parAlign     = Justify)
      .copy(fontFamily=Family("Arial"))
      .fontSize(22)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)
      .frameStyle(Decoration.Blurred(fg=blue, blur=15f, spread=15f), fg=white)
      .paragraphEms(45)
      //.frameStyle(decoration=Decoration.Shaded(fg=blue, bg=darkGrey))

  val t1 = Text("""GlyphML is a domain-specific language embedded in Scala: its elements denote Glyphs.
                  |
                  |Its API may  be somewhat more convenient for interface designers than the standard Glyphs API.
                  |
                  |""".stripMargin)(paragraphEms(40))(paragraphEms(30))

  val t2 = Text("""This is an experiment in choosing the details of GUI layout dynamically.
                  |
                  |In row layout these two paragraphs appear beside each other in their "natural" sizes
                  |for the current context. In column layout they will appear as a column.
                  |
                  |As the window geometry is changed manually the GUI may switch between row and
                  |column layout, and the initial "welcome", which is just an
                  |invitation to choose.
                  |
                  |""".stripMargin)(paragraphEms(25))

  val r1 = Cached(Local)(t1.framed(fg=DefaultBrushes.black strokeWidth 2, bg=lightGrey))
  val r2 = Cached(Local)(t2.framed(fg=DefaultBrushes.black strokeWidth 2, bg=lightGrey))
  val anchor = r1

  lazy val traceOn: Glyph = styled.TextButton("Trace on") {
    _ =>
      val root = traceOn.findRoot
      root.decodeMotionModifiers(true)
      root.trackMouseState(true)
      RootGlyph.level=FINE
  }(Local.style)

  lazy val traceOff: Glyph = styled.TextButton("Trace off") {
    _ =>
      val root = traceOff.findRoot
      root.decodeMotionModifiers(false)
      root.trackMouseState(false)
      RootGlyph.level=FINE
  }(Local.style)

  lazy val rowLayout: Glyph = styled.TextButton("Row layout") {
    _ => rowLayout.findRoot.rootWindowResized(r1.originalW+r2.originalW+(traceOn.w max traceOff.w)+40, r1.originalH+r2.originalH, force=true)
  }(Local.style)

  var dynamicLayout: Boolean = false

  lazy val dynaLayout: Glyph = styled.CheckBox(dynamicLayout) {
    state =>
      dynamicLayout=state
      RootGlyph.fine(s"dynamicLayout $dynamicLayout")
  }(Local.style)

  lazy val colLayout: Glyph = styled.TextButton("Column layout") {
    _ => colLayout.findRoot.rootWindowResized(r1.originalW max r2.originalW+(traceOn.w max traceOff.w), (r1.originalH+r2.originalH)*1.5f, force=true)
  }(Local.style)

  val GUI: Glyph = Resizeable(Local) {

    def menuBar =  MenuBar(Local)(dynaLayout, traceOn(), Gap, traceOff)
    def sideBar =  SideBar(Local)(dynaLayout, traceOn(), Gap, traceOff)
    def sizeBar =  Column(rowLayout, colLayout, traceOn, dynaLayout)

    def splitRow(w: Scalar, h: Scalar) = {
      FixedWidth(w)(sideBar, Glyphs.FilledRect(6, h, darkGrey), r1, Gap, Glyphs.FilledRect(6, h, nothing), Gap, r2)
    }

    def HalfRow(w: Scalar, h: Scalar, g1: Element, g2: Element) = {
      RootGlyph.fine(s"HalfRow $w")
      FixedWidth(w)(g1, Gap, g2)
    }

    def splitCol(w: Scalar, h: Scalar) = {
      FixedHeight(h)(menuBar, r1, Gap, r2)
    }

    Dynamic {
      case Vec(w, h) if (dynamicLayout && w>650f) =>
        RootGlyph.fine(s"Dynamic $w")
        HalfRow(w, h,
          t2.locally(paragraphPoints(w/3f)), t1(paragraphPoints(w/4f)))

      case Vec(w, h) if w>=(traceOn.w max traceOff.w)+r1.originalW+r2.originalW => splitRow(w, h)
      case Vec(w, h) if h>=traceOn.h+r1.originalH+r2.originalH => splitCol(w, h)
      case _ => sizeBar.framed(fg=red, bg=nothing)
      }
  }

  override def title: String = "Topdown Example"

  override val defaultIconPath: Option[String] = Some("./flag.png")
}
