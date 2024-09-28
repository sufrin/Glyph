package org.sufrin.glyph
package tests

import GlyphML._
import Glyphs._
import GlyphTypes.Scalar
import Styles.{Decoration, GlyphStyle}

import org.sufrin.logging.{FINE, OFF}

class GlyphMLTest { }

object GlyphMLTest extends Application {

  object LocalStyle extends Styles.DefaultSheet {
    override def buttonFontSize: Scalar  = 22
    override def labelFontSize: Scalar   = 22
    override def labelStyle: GlyphStyle  = GlyphStyle(labelFont, buttonStyle.up.fg, buttonStyle.up.bg)
  }

  val Local: Context =
    Context(
      style          = LocalStyle,
      paragraphWidth = 400,
      boundingBox    = Vec(400, 250),
      leftMargin     = 0,
      rightMargin    = 0,
      parAlign       = Justify,
      fg             = Glyphs.black)
      .copy(fontFamily=FontFamily("Menlo"))
      .fontSize(20)
      .labelStyle(fg=Glyphs.black, bg=Glyphs.nothing)
      .gridStyle(bg=Glyphs.nothing, fg=nothing, padY=8, padX=8)
      .frameStyle(Decoration.Blurred(fg=blue, blur=15f, spread=15f), fg=white)
      .paragraphEms(45)
      //.frameStyle(decoration=Decoration.Shaded(fg=blue, bg=darkGrey))


  val text1 = Text("""
                  |GlyphML is a domain-specific language embedded in Scala: its elements denote Glyphs.
                  |
                  |Its API may  be somewhat more convenient for interface designers than the standard Glyphs API.
                  |
                  |This is an experiment in choosing the details of GUI layout dynamically. It
                  |explores several different possibilities, not all of which would usually be
                  |adviseable in a production GUI.
                  |
                  |
                  |""".stripMargin).framed(fg=DefaultBrushes.black strokeWidth 2, bg=lightGrey)

  val text2 = Text("""
                  |
                  |In row layout these two paragraphs appear beside each other in their "natural" sizes
                  |for the current context. In column layout they will appear as a column.
                  |
                  |As the window geometry is resized manually the GUI may switch between the initial splash screen, and row and
                  |column layout, with texts at their "natural" widths. Once the cursor moves after the resizing, the window
                  |is made to surround the GUI exactly.
                  |
                  |When "Dynamic" is enabled and the window frame has been made somewhat larger than
                  |the splash screen, text widths are chosen dynamically.
                  |
                  |""".stripMargin).framed(fg=DefaultBrushes.black strokeWidth 2, bg=lightGrey)

    // The texts at their natural (predefined) sizes.
    val nat1 = Constant(text1.locally(paragraphEms(30)))(Local)
    val nat2 = Constant(text2.locally(paragraphEms(50)))(Local)
    val anchor = nat1

    lazy val traceOn: Glyph = styled.TextToggle(whenFalse="Trace on", whenTrue="Trace off", initially=false) {
      state =>
        val root = traceOn.guiRoot
        root.decodeMotionModifiers(state)
        root.trackMouseState(state)
        RootGlyph.level= if (state) FINE else OFF
    }(Local.style)

    var dynamicLayout: Boolean = false

    lazy val dynaLayout: Glyph = styled.TextToggle(whenFalse="Enable Dynamic", whenTrue="Disable Dynamic", initially=dynamicLayout) {
      state =>
        dynamicLayout=state
        RootGlyph.fine(s"dynamicLayout $dynamicLayout")
    }(Local.style)

    val GUI: Glyph = Resizeable(Local) {


    lazy val rowLayout: Glyph = styled.TextButton("Row layout") {
      _ =>
        val root = rowLayout.guiRoot
        root.rootWindowResized(rowW, rowH, force=true)
    }(Local.style)

    lazy val colLayout: Glyph = styled.TextButton("Column layout") {
      _ =>
        val root = colLayout.guiRoot
        root.rootWindowResized(rowW, colH+60, force=true)
    }(Local.style)

    lazy val splashLayout: Glyph = styled.TextButton("Splash Screen") {
        _ =>
          val root = splashLayout.guiRoot
          root.rootWindowResized(splashScreen.w+2*enlarge, splashScreen.h+2*enlarge, force=true)
    }(Local.style)

    def sideBar                    = SideBar(Local)(dynaLayout, Gap, rowLayout, colLayout, splashLayout)
    lazy val sideBarWidth:  Scalar = 4+dynaLayout.w max rowLayout.w max colLayout.w max splashLayout.w
    def menuBar                    = MenuBar(Local)(dynaLayout, Gap, rowLayout, colLayout, splashLayout)
    lazy val menuBarHeight: Scalar = dynaLayout.h max rowLayout.h max colLayout.h max splashLayout.h

    lazy val splashColumn: Element  = Centered(rowLayout, colLayout, traceOn, dynaLayout)
    lazy val splashScreen: Constant = splashColumn.constant(Local)

    def layoutRow(w: Scalar, h: Scalar) =
        FixedWidth(w)(sideBar, Glyphs.FilledRect(2, h, darkGrey), nat1, Gap, Glyphs.FilledRect(2, h, nothing), Gap, nat2)

      def layoutCol(w: Scalar, h: Scalar) =
          FixedHeight(h-2*enlarge)(menuBar, nat1, Gap, nat2)

      def layoutDynamic(w: Scalar, h: Scalar, g1: Element, g2: Element) = {
          RootGlyph.fine(s"layoutDynamic $w")
          FixedWidth(w-2*enlarge)(sideBar, g1, Gap, g2)
      }

      lazy val enlarge: Scalar  = 10f
      lazy val rowW: Scalar = sideBarWidth+nat1.w+nat2.w+2*enlarge+2
      lazy val rowH: Scalar = sideBarWidth+(nat1.h max nat2.h)+2*enlarge+2
      lazy val colH: Scalar = menuBarHeight+nat1.h+nat2.h+2*enlarge

      var selectCount = 0

      def selectDynamic(windowSize: Vec): Element = {
        windowSize match {
          case Vec(w, h) if dynamicLayout && h>splashScreen.h*1.2f && w>splashScreen.w*1.5f =>
            RootGlyph.fine(s"Dynamic $w")
            val ww = w-sideBarWidth-2*enlarge
            layoutDynamic(w, h, text1.original(paragraphPoints(ww*0.3f)).framed(fg=redLine, bg=lightGrey),
              text2.original(paragraphPoints(ww*0.5f)).framed(fg=redLine, bg=lightGrey))
          case Vec(w, h) if w>=rowW && h<colH => layoutRow(w, h)
          case Vec(w, h) if h>=colH           => layoutCol(w, h)
          case Vec(w, h) =>
            Resizeable.finest(s"splashScreen selected for ($w, $h) => (${splashScreen.w}, ${splashScreen.h})")
            splashScreen
        }
      }

      Dynamic(selectDynamic, enlarge=10f)
  }

  override def title: String = "Topdown Example"

  override val defaultIconPath: Option[String] = Some("./flag.png")
}
