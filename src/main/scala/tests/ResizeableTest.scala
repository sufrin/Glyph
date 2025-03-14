package org.sufrin.glyph
package tests

import NaturalSize.{Col, Row}

import org.sufrin.glyph.DefaultBrushes.{black, blackFrame, redFrame}
import org.sufrin.glyph.ReactiveGlyphs.TextButton
import org.sufrin.glyph.styled.Label
import org.sufrin.glyph.styles.decoration.Edged
import org.sufrin.glyph.Location.East

import scala.xml.Elem

object ResizeableTest extends Application {
  import glyphXML.Language._



  def guiSpec(implicit style: StyleSheet): Glyph = Col(align=Center)(
    { import style.{hFill,em}
      val Vec(w, h) = style.windowDiagonal
      lazy val wider: Glyph = styled.TextButton("Wider") {
        _ => wider.guiRoot.setContentSize(Vec(w*1.2f, h))
      }
      lazy val narrower: Glyph = styled.TextButton("Narrower") {
        _ => wider.guiRoot.setContentSize(Vec(w*0.9f, h))
      }
      lazy val about: Glyph = styled.TextButton("About") {
        _ => styled.windowdialogues.Dialogue.FLASH(
          <div width="50em" align="justify" fontScale="0.8" frame="white/16">
            <p>
              This application is designed to demonstrate the <b>Resizeable</b> glyph constructor.
              You can make its window wider (narrower) by pressing the appropriate buttons, or by
              dragging the edge of the window.
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
        FixedSize.Row(w).Mid(wider, em, narrower, hFill(), Label(s"[$w]"), hFill(), about),
        Glyphs.Rect(w, 2, fg=black)
      )
    },
    <div width="1*windowwidth" align="justify">
      <p align="center" fontScale="2">De Rerum Natura</p>
      <p>
        De Rerum Natura is a beau_tiful small yarn producer in France
        The wool used is a blend of French and Euro_pean merino raised with con_cern for the well_being of an_imals and the rec_ov_ery of their wool.
        All man_ufact_uring is carr_ied out in France with small flour_ishing mills and the ut_most care and con_cern for the env_iron_ment.
        The res_ult is yarns that are as soft and beau_tiful to knit with as they are to wear.
      </p>
      <p>
        All De Rerum Natura bases are <row>mulesing<span textForeground="red" fontScale="0.5">(*)</span></row> free, and ethically and sustainably produced.
      </p>
      <fill width="1*windowwidth" foreground="red/1~5~5"  height="0.7ex"/>
      <p fontScale="0.7" labelForeground="red" textForeground="red" hang="* ">
        Mulesing is the removal of strips of wool-bearing skin from around the breech of a
        sheep to prevent the parasitic infection flystrike. The wool around the buttocks can retain feces and urine, which attracts flies.
      </p>
    </div>
  ) enlarged 10 edged (black(width=2)) enlarged 20

  import DefaultBrushes._
  implicit val style: StyleSheet = StyleSheet(
    windowDiagonal        = Vec(400,150),
    buttonDecoration      = Edged(redFrame),
    labelBackgroundBrush  = white,
    textBackgroundBrush   = white,
    backgroundBrush       = white,
    parSkip               = 10f
  )

  val GUI: Glyph = styled.Resizeable(style => guiSpec(style))

  def title: String = "Resizeable Test"

  override def whenStarted(): Unit = {
    println(s"STARTED $GUI")
    Application.confirmCloseRequestsFor(GUI)
    styled.Resizeable.loggingLevel(org.sufrin.logging.ALL)
  }
}