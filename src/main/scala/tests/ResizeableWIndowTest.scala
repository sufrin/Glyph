package org.sufrin.glyph
package tests

import NaturalSize.{Col, Row}

import org.sufrin.glyph.DefaultBrushes.{black, blackFrame, redFrame}
import org.sufrin.glyph.ReactiveGlyphs.TextButton
import org.sufrin.glyph.styled.{Label, Resizeable}
import org.sufrin.glyph.styles.decoration.Edged
import org.sufrin.glyph.Glyphs.{Concentric, FilledRect, Rect}
import org.sufrin.glyph.Location.East

import scala.xml.Elem

object ResizeableWIndowTest extends Application {
  import glyphXML.Language._



  def guiSpec(implicit style: StyleSheet): Glyph = Col(align=Center)(
    { import style.{hFill,em}
      val Vec(w, h) = style.containerDiagonal
      lazy val wider: Glyph = styled.TextButton("Wider", Hint(1, "Make the window 10% wider")) {
        _ => wider.guiRoot.setContentSize(Vec(w*1.1f, h))
      }
      lazy val bigger = styled.TextButton("Bigger", Hint(1, "Make the font bigger")) {
        _ =>
          wider.guiRoot.GUIroot match {
            case resizeable: Resizeable =>
              resizeable.currentStyle = resizeable.currentStyle.copy(textFontSize=resizeable.currentStyle.textFontSize*1.1f)
              Resizeable.info(s"[Bigger] ${resizeable.currentStyle.textFontSize}")
              // force recalculation
              resizeable.forceResize()
            case other =>
          }
        }
      lazy val smaller = styled.TextButton("Smaller", Hint(1, "Make the font smaller")) {
        _ =>
          wider.guiRoot.GUIroot match {
            case resizeable: Resizeable =>
              resizeable.currentStyle = resizeable.currentStyle.copy(textFontSize=resizeable.currentStyle.textFontSize*0.9f)
              Resizeable.info(s"[Smaller] ${resizeable.currentStyle.textFontSize}")
              resizeable.forceResize()
            case other =>
          }
      }
      lazy val narrower: Glyph = styled.TextButton("Narrower", Hint(1, "Make the window 10% narrower")) {
        _ => narrower.guiRoot.setContentSize(Vec(w*0.9f, h))
      }
      lazy val about: Glyph = styled.TextButton("About") {
        _ => styled.windowdialogues.Dialogue.OK(
          <div width="50em" align="justify" fontScale="0.8" frame="white/16">
            <p>
              This application is designed to demonstrate the <b>Resizeable</b> glyph constructor.
              You can make its window wider (narrower) by pressing the appropriate buttons, or by
              dragging the edge of the window. You can also change the font size to make it bigger or smaller
              within the same window.
            </p>
            <p>
              After the window size is changed, the app gives up the mouse and terminal focus; so
              if you want to press buttons again, just (leave and) re-enter the window.
            </p>
          </div>,
          East(about) // this  MUST be here
        ).start()
      }
      Col(
        FixedSize.Row(w).Mid(wider, em, bigger, em, narrower, em, smaller, hFill(), Label(s"[$w]"), hFill(), about),
        Glyphs.Rect(w, 2, fg=black)
      )
    },
    <div width="1*container.width" align="justify">
      <p align="center" fontScale="2">De Rerum Natura</p>
      <p>
        De Rerum Natura is a beau_tiful small yarn producer in France
        The wool used is a blend of French and Euro_pean merino raised with con_cern for the well_being of an_imals and the rec_ov_ery of their wool.
        All man_ufact_uring is carr_ied out in France with small flour_ishing mills and the ut_most care and con_cern for the env_iron_ment.
        The res_ult is yarns that are as soft and beau_tiful to knit with as they are to wear.
      </p>
      <p>
        All De Rerum Natura bases are mule_sing<row><span textForeground="red" fontScale="0.5">(*)</span></row> free, and eth_ical_ly and sus_tain_ably produced.
      </p>
      <fill width="1*container.width" foreground="red/1~5~5"  height="0.7ex"/>
      <p fontScale="0.7" labelForeground="red" textForeground="red" hang="* ">
        Mulesing is the removal of strips of wool-bearing skin from around the breech of a
        sheep to prevent the parasitic infection flystrike. The wool around the buttocks can retain feces and urine, which attracts flies.
      </p>
    </div>
  ) enlarged 10 edged (black(width=2)) enlarged 20

  import DefaultBrushes._

  implicit val style: StyleSheet = StyleSheet(
    containerDiagonal     = Vec(400,150),
    buttonDecoration      = Edged(red(width=2).sliced(3,1)),
    labelBackgroundBrush  = white,
    textBackgroundBrush   = white,
    backgroundBrush       = white,
    parSkip               = 10f
  )

  /**
   * The GUI is recalculated by the functional argument to `Resizeable` whenever the window size changes, or the
   * (text) font size changes. The function maps the current context (which
   * starts off as the original `style`) into the GUI Glyph.
   *
   * Here the function replaces the GUI by a single button if  the window gets too narrow.
   */
  val GUI: Glyph = styled.Resizeable {

    // the window size is too small
    case context: StyleSheet if context.containerWidth < 25*context.emWidth =>
      lazy val wider: Glyph = styled.TextButton("Wider") {
        _ =>
          Resizeable.fine(s"[Wider] pressed  ${context.containerDiagonal}")
             wider.guiRoot.setContentSize(Vec(context.containerWidth*1.5f, context.containerHeight))
      }
      Resizeable.fine(s"[Wider]-button-only GUI  ${context.windowDiagonal} (25em is ${25*context.emWidth})")
      Col(align=Center)(wider).edged(red(width=4))

    case context: StyleSheet =>
      guiSpec(context)
  }

  def title: String = "Resizeable Test"

  override def whenStarted(): Unit = {
    Application.confirmCloseRequestsFor(GUI)
  }
}