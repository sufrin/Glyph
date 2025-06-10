package org.sufrin.glyph
package tests

import styled.{Book, BookSheet, TextButton}
import Brushes._
import GlyphTypes.Scalar
import NaturalSize.{Col, Row}
import styled.Label.Label
import styles.decoration.Edged
import unstyled.dynamic.SplitScreen
import unstyled.static._

import io.github.humbleui.jwm.EventKey

/**
 * Demonstrates that a SplitScreen can function properly.
 */
object ResizeAndSplitTest extends Application {
  val protoStyle: StyleSheet = StyleSheet(
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
  implicit val  style: StyleSheet = protoStyle.copy(containerDiagonal = Vec(70*protoStyle.emWidth, 0))

  import glyphXML.Language._

  import style.ex
  val title: String = "Resize and Split Tests"

  val SplitTest: Glyph = {

    val enlargement: Scalar = 30f

    import org.sufrin.glyph.unstyled.reactive.Slider

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
    def blob    = FilledRect(28f, 14f, fg=black.blurred(6f))
    val slider  = Slider.Horizontal(Rect(dynamic.w, 2f), blob, dynamic.proportion){
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
      <p align="center" fontScale="0.7">{s"${new java.util.Date()}"}</p>
      <p align="center" fontScale="2">De Rerum Natura</p>
      <p>
        De Rerum Natura is a beau_tiful small yarn producer in France.
        The wool used is a blend of French and Euro_pean merino raised with con_cern for the well_being of an_imals and the rec_ov_ery of their wool.
        All man_ufact_uring is carr_ied out in France with small flour_ishing mills and the ut_most care and con_cern for the env_iron_ment.
        The res_ult is yarns that are as soft and beau_tiful to knit with as they are to wear.
      </p>
      <p>
        All De Rerum Natura bases are mule_sing<row><span textForeground="red" fontScale="0.5">(*)</span></row> free, and eth_ical_ly and sus_tain_ably produced.
      </p>
      <fill width="1*container.width" foreground="red.sliced(1,5)"  height="0.7ex"/>
      <p fontScale="0.7" labelForeground="red" textForeground="red" hang="* ">
        Mulesing is the removal of strips of wool-bearing skin from around the breech of a
        sheep to prevent the parasitic infection flystrike. The wool around the buttocks can retain feces and urine, which attracts flies.
      </p>
      <fill width="1*container.width" foreground="red.sliced(1,5)"  height="0.7ex"/>
      <div align="center" fontSkip="0ex">
      <p fontScale="0.4" >{s"container: ${style.containerDiagonal}"}</p>
      <p fontScale="0.4" >{s"window: ${style.windowDiagonal}"}</p>
      <p fontScale="0.4" >{s"screen: ${style.screenDiagonal}"}</p>
      </div>
    </div>

    lazy val resizeable = styled.Resizeable {
      case context: StyleSheet =>
        theText(context)
    }

    val viewPort = unstyled.dynamic.ViewPort(resizeable, fg=redFrame, initialPortDiagonal = resizeable.diagonal)

    val widths = List(80, 70, 60, 50, 40, 30, 20)
    val radioWidths  = styled.RadioCheckBoxes(widths.map{ems => s"${ems}ems"}, prefer = "70ems"){
        case None       =>
        case Some(box)  =>
          viewPort.reset()
          resizeable.atSize(Vec(widths(box)*style.emWidth, 0f))
      }

    val scales = List(0.3f, 0.5f, 0.8f, 1.0f, 1.2f, 1.5f, 1.7f)
    val radioScales = styled.RadioCheckBoxes(scales.map{ scale => s"*$scale"}, prefer="*1.0") {
      case None =>
      case Some(index) =>
        viewPort.reset()
        resizeable.currentStyle = resizeable.currentStyle.copy(textFontSize=protoStyle.textFontSize*scales(index))
        resizeable.atSize(resizeable.currentStyle.containerDiagonal)
    }
    Col(align=Center)(
        FixedSize.Row(width=SplitTest.w, align=Mid)(
          Col(align=Center)(radioWidths.glyphButtons()),
          style.hFill(),
          viewPort,
          style.hFill(),
          Col(align=Center)(radioScales.glyphButtons(align=Left)),
        ),
        style.vFill(3),
      <p width={s"${SplitTest.w}px"} align="justify">
        This page demonstrates the interaction between a viewport and a dynamically resizeable glyph. The subject matter of
        the text is rendered afresh whenever a width checkbox is selected, or a multiple of the original font size is clicked.
        The viewport can be navigated by using the mousewheel or directional arrows.
        (Widths are expressed in ems in the original font size).
      </p> enlarged 20 edged(red(width=1).sliced(5,5)) enlarged 10
    )
  }


  def genGUI: StyleSheet => Glyph = {
    case context =>
      implicit val bookSheet: BookSheet = BookSheet(context, context)
      val book = Book()
      val Page = book.Page
      Page("Resize") { ResizeTest }
      Page("Split")  { SplitTest }
      book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)
  }


  val GUI: Glyph = genGUI(style)

  override val defaultIconPath: Option[String] = Some("cherub.png")

  override def whenStarted(): Unit = {
    Application.confirmCloseRequestsFor(GUI)
  }

  override def handleUnfocussedKey(event: EventKey): Unit = {}
}
