package org.sufrin.glyph
package tests

import NaturalSize.Col
import styled.{Label, Resizeable}
import styles.decoration.Edged
import Location.East
import glyphML.{TransformsPackage, Translator}
import unstyled.static

object ResizeableWindowTest extends Application {
  import Brushes._

  implicit val style: StyleSheet = StyleSheet(
    containerDiagonal     = Vec(400,150),
    buttonDecoration      = Edged(red(width=2).sliced(3,1)),
    labelBackgroundBrush  = white,
    textBackgroundBrush   = white,
    backgroundBrush       = white,
    parSkip               = 10f
    )

  val translator = Translator().withPackage(TransformsPackage)

  locally {
    import translator.definitions
    definitions("dagger") = "\u2020"
    definitions("ddagger") = "\u2021"
    definitions("dddagger") = "\u2e38"
  }

  val superscripts = "¹²³⁴⁵⁶⁷⁸⁹⁰⁽⁾"

  val helpText: Glyph = {
    val language = translator(style)
    import language._

    <div width="50em" align="justify" fontScale="0.8" frame="white/16">
      <p>
        This application is designed to demonstrate the <b>Resizeable</b> glyph constructor
        working together with <b>glyphXML</b>.
        You can make its window wider (narrower) by pressing the appropriate buttons, or by
        dragging the edge of the window. You can also change the font size to make it bigger or smaller
        within the same window.
      </p>
      <p>
        After the window width is changed and the text laid out again, the app gives up the mouse and terminal focus; so
        if you want to press buttons again, just (leave and) re-enter the window.
      </p>
    </div>
  }

  /**
   *  This is defined here, outwith the scope of the `guiSpec` function, because
   *  the `about` button and its dialogue remain the same size, independently of
   *  the size of the application's window.
   */
  lazy val about: Glyph = styled.TextButton("About") {
    _ => styled.windowdialogues.Dialogue.OK(helpText, East(about)).start()
  }

  def guiSpec(implicit style: StyleSheet): Glyph = {

    val buttons = {
      import style.{em, hFill}
      lazy val wider: Glyph = styled.TextButton("10%\n⭅⭆", Hint(1, "column wider")) {
        _ => wider.guiRoot.setContentSize(Vec(style.containerWidth * 1.1f, style.containerHeight))
      }
      lazy val bigger = styled.TextButton("+10%", Hint(1, "font size")) {
        _ =>
          wider.guiRoot.GUIroot match {
            case resizeable: Resizeable =>
              resizeable.currentStyle = resizeable.currentStyle.copy(textFontSize =
                                                                       resizeable.currentStyle.textFontSize * 1.1f)
              Resizeable.info(s"[Bigger] ${resizeable.currentStyle.textFontSize}")
              // force recalculation
              resizeable.forceResize()
            case other =>
          }
      }
      lazy val smaller = styled.TextButton("-10%", Hint(1, "font size")) {
        _ =>
          wider.guiRoot.GUIroot match {
            case resizeable: Resizeable =>
              resizeable.currentStyle = resizeable.currentStyle.copy(textFontSize =
                                                                       resizeable.currentStyle.textFontSize * 0.9f)
              Resizeable.info(s"[Smaller] ${resizeable.currentStyle.textFontSize}")
              resizeable.forceResize()
            case other =>
          }
      }
      lazy val narrower: Glyph = styled.TextButton("10%\n⭆⭅", Hint(1, "column narrower")) {
        _ => narrower.guiRoot.setContentSize(Vec(style.containerWidth * 0.9f, style.containerHeight))
      }
      Col(align = Center)(
        FixedSize.Row(style.containerWidth).Mid(wider, em, narrower, hFill(),
                                                Label(s"[${
          (style.containerWidth / style.emWidth).toInt
        }ems]"), hFill(), bigger, em, smaller, em, about),
        style.ex,
        static.Rect(style.containerWidth, 2, fg = black)
        )
    }

    val text: Glyph = {
      val language = translator(style)
      import language._
      <div width="container.width" align="justify" macrowarning="off">
        <macro tag="strut"><fill width="0em" stretch="0" height="3ex"/></macro>
        <macro tag="emfill"><fill width="0em" stretch="20"/></macro>
        <macro tag="sup"><row alignment="top"><strut/><span textforeground="red" fontscale="0.6"><b><?body?></b></span></row></macro>
        <macro tag="centered"><row width="width"><emfill/><?body?><emfill/></row></macro>

         <p align="center" fontScale="2">De Rerum Natura</p>
         <p>
           De Rerum Natura is a beau_tiful small yarn producer in France
           The wool used is a blend of French and Euro_pean merino raised with con_cern for the well_being of an_imals and the rec_ov_ery of their wool.
           All man_ufact_uring is carr_ied out in France with small flour_ishing mills and the ut_most care and con_cern for the env_iron_ment.
           The res_ult is yarns that are as soft and beau_tiful to knit with as they are to wear.
         </p>
         <p>
           All De Rerum Natura bases are free of mule_sing<sup>&ddagger;</sup> and are eth_ical_ly   and sus_tain_ably produced.
         </p>

         <p>
           <i>Try changing the
             <bi>width</bi>
             of this window by dragging a vertical edge.</i>
         </p>
         <strut/>
         <centered>
          <frame fg="blue.2" radius="0.05" enlarge="30">
            <p width="0.7*width" fontScale="0.7" hang="‡ " labelForeground="red" textForeground="red">
              Mulesing is the removal of strips of wool-bearing skin from around the breech of
              sheep to prevent the parasitic infection
              <i>flystrike.</i>
              The wool around the buttocks can retain faeces and urine, which attracts flies.
            </p>
          </frame>
         </centered>
       </div>
    }
    Col(align=Center)(buttons, text) enlarged 10 edged (black(width = 2)) enlarged 20
  }




  /**
   * The GUI is recalculated by the functional argument to `Resizeable` whenever the window size changes, or the
   * (text) font size changes. The function maps the current context (which
   * starts off as the original `style`) into the GUI Glyph.
   * Here it replaces the GUI by a single button if the window gets too narrow.
   */
  val GUI: Glyph = styled.Resizeable {
    // the window size is too small: replace its content with a functional button
    case context: StyleSheet if context.containerWidth < 25*context.emWidth =>
      lazy val wider: Glyph = styled.TextButton(f"[${(context.containerWidth/context.emWidth).toInt}ems is too narrow]\nClick or drag to Widen") {
        _ =>
          Resizeable.fine(s"[Wider] pressed  ${context.containerDiagonal}")
          wider.guiRoot.setContentSize(Vec(1.2*25*context.emWidth, context.containerHeight))
      }
      Resizeable.fine(s"[Wider]-button-only GUI  ${context.windowDiagonal} (25em is ${25*context.emWidth})")
      Col(align=Center)(wider).edged(red(width=4))

    case context: StyleSheet =>
      guiSpec(context)
  }(style)

  def title: String = "Resizeable Test"

  override def whenStarted(): Unit = {
    Application.confirmCloseRequestsFor(GUI)
  }
}