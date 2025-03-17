package org.sufrin.glyph
package tests

import styled.{Book, BookSheet, Paragraph, TextButton}
import DefaultBrushes.{black, blackFrame, buttonPointSize, darkGrey, red, white}
import GlyphTypes.Scalar
import NaturalSize.{Col, Row}
import dynamic.SplitScreen

import io.github.humbleui.jwm.EventKey
import org.sufrin.glyph.styles.decoration.Edged
import org.sufrin.glyph.Glyphs.Image
import org.sufrin.glyph.styled.Label.Label

/**
 * Demonstrates that a SplitScreen can function properly.
 */
object ResizeAndSplitTest extends Application {
  implicit val style: StyleSheet = StyleSheet(
    buttonFontSize        = 25f,
    textFontSize          = 25f,
    labelFontSize         = 25f,
    containerDiagonal     = Vec(800,0),
    buttonDecoration      = Edged(red(width=2).sliced(3,1)),
    labelBackgroundBrush  = white,
    textBackgroundBrush   = white,
    backgroundBrush       = white,
    parSkip               = 10f
  )
  import style.ex
  import glyphXML.Language._
  val title: String = "Resize and Split Tests"

  val SplitTest: Glyph = {

    val enlargement: Scalar = 30f

    import ReactiveGlyphs.Slider

    lazy val enableDynamic: Glyph = styled.CheckBox(initially=false) {
      state => enableDynamic.guiRoot.autoScale = state
    }

    val left = <div width="30em" align="justify">
      <p>This is a justified piece of text that may be quite small.
      You'll see it on a split screen. When the text on the other
      screen is not the same width we'll see what happens.
      </p>
      <p>
        The main point to note is that the button loses focus when
        the pointer moves into a region where it is occluded.
      </p>
    </div> above
      styled.TextButton("The Left Button") { _ => println("LEFT") }
    val right = <div width="40em" align="left">
      <p>This is a left-justified piece of text that may be quite small.
        You'll see it on a split screen. It'll be a bit wider
        than the other thing on the screen.
      </p>
      <p>
        The main point to note is that the button loses focus when
        the pointer moves into a region where it is occluded.
      </p>
    </div> above
      styled.TextButton("The Right Button") { _ =>  println("RIGHT") }

    val dynamic = SplitScreen(left enlarged 30, right enlarged 30, dynamic=true, fg=darkGrey.strokeWidth(6f))
    def blob    = Glyphs.FilledRect(28f, 14f, fg=black.blurred(6f))
    val slider  = Slider.Horizontal(Glyphs.Rect(dynamic.w, 2f), blob, dynamic.proportion){
      case proportion: Scalar => dynamic.setBoundary(proportion)
    }

    val static = SplitScreen(left() enlarged 30, right() enlarged 30, dynamic=false, fg=darkGrey.strokeWidth(6f))

    Col(align=Center)(
      dynamic,
      slider,
      TextButton("<"){
        _ => dynamic.setBoundary(0.0f); slider.dragTo(0f)
      } beside
        TextButton("<>") {
          _ => dynamic.exchange()
        } beside
        TextButton(">"){
          _ => dynamic.setBoundary(1.0f); slider.dragTo(0.999f)
        },
      ex, ex, ex,
      static,
      TextButton("L<->R") {
        _ => static.exchange()
      },
      Row(align=Mid)(Label("Enable dynamic scaling:"), enableDynamic)
    ) enlarged enlargement framed black(width=4)
  }

  val ResizeTest: Glyph = {

    def theText(implicit style: StyleSheet): Glyph= <div width="1*container.width" align="justify">
      <p align="center" fontScale="2">De Rerum Natura</p>
      <p>
        De Rerum Natura is a beau_tiful small yarn producer in France.
        The wool used is a blend of French and Euro_pean merino raised with con_cern for the well_being of an_imals and the rec_ov_ery of their wool.
        All man_ufact_uring is carr_ied out in France with small flour_ishing mills and the ut_most care and con_cern for the env_iron_ment.
        The res_ult is yarns that are as soft and beau_tiful to knit with as they are to wear.
      </p>
      <p>
        All De Rerum Natura bases are <row>mulesing<span textForeground="red" fontScale="0.5">(*)</span></row> free, and eth_ical_ly and sus_tain_ably produced.
      </p>
      <fill width="1*container.width" foreground="red/1~5~5"  height="0.7ex"/>
      <p fontScale="0.7" labelForeground="red" textForeground="red" hang="* ">
        Mulesing is the removal of strips of wool-bearing skin from around the breech of a
        sheep to prevent the parasitic infection flystrike. The wool around the buttocks can retain feces and urine, which attracts flies.
      </p>
      <fill width="1*container.width" foreground="red/1~5~5"  height="0.7ex"/>
      <p fontScale="0.7">
        Containers: {s"${style.screenDiagonal} ${style.windowDiagonal} ${style.containerDiagonal}"}
      </p>
    </div>

    lazy val resizeable = styled.Resizeable {
      case context: StyleSheet =>
        val image = new Image(theText(context))
        image enlarged 20 edged red enlarged 20
    }

    val viewPort = dynamic.ViewPort(resizeable, blackFrame)

    FixedSize.Row(width=SplitTest.w, align=Mid)(
      Col(
        styled.TextButton("800") {
          _ =>
            viewPort.reset()
            resizeable.atSize(Vec(800f, 0f))
        },
        styled.TextButton("500") {
          _ =>
            viewPort.reset()
            resizeable.atSize(Vec(600f, 0f))
        },
        styled.TextButton("300") {
          _ =>
            viewPort.reset()
            resizeable.atSize(Vec(300f, 0f))
        }),
      style.hFill(),
      viewPort,
      style.hFill(),

    )
  }


  def genGUI: StyleSheet => Glyph = {
    case context =>
      implicit val bookSheet: BookSheet = BookSheet(context, context)
      val book = Book()
      val Page = book.Page
      Page("Split")  { SplitTest }
      Page("Resize") { ResizeTest }
      book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)
  }


  val GUI: Glyph = genGUI(style)

  override def handleUnfocussedKey(event: EventKey): Unit = {}
}
