package org.sufrin.glyph
package tests.demonstrationBook

import styled.{BookSheet, RadioCheckBoxes, TextButton}
import NaturalSize._

import io.github.humbleui.skija.PaintStrokeCap
import org.sufrin.glyph.styles.ButtonStyle

class  SplashScreen(implicit sheet: BookSheet, implicit val translator: glyphXML.Translation) {
  implicit val  buttons: StyleSheet =
                sheet.pageSheet.copy(buttonFrame = styles.decoration.Edged(sheet.pageSheet.buttonForegroundBrush(width=5, cap=PaintStrokeCap.ROUND), sheet.pageSheet.buttonBackgroundBrush))
  import buttons.{em,ex}

  import GlyphTypes.Window
  val GUI: Glyph = {

    lazy val Duplicated = new Interface with Application {
      val GUI: Glyph =
        if      (extraArgs contains "-notebook")   asLNotebook
        else if (extraArgs contains "-rnotebook")  asRNotebook
        else if (extraArgs contains "-lnotebook")  asLNotebook
        else if (extraArgs contains "-snotebook")  asSNotebook
        else if (extraArgs contains "-vnotebook")  asVNotebook
        else if (extraArgs contains "-tnotebook")  asTNotebook
        else if (extraArgs contains "-cnotebook")  asCheckBoxes
        else asLNotebook

      def title = s"""Demonstration Book -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

      override
      val defaultIconPath: Option[String] = Some("./flag.png")

      override
      def handleWindowCloseRequest(window: Window): Unit = confirmCloseOn(GUI)(window)

    }

    var style: String = "-notebook"
    var scale: String = "-scale=0.8"
    var screen: String = "-screen=p"
    val styles  = "-notebook/-cnotebook/-rnotebook/-snotebook/-vnotebook/-tnotebook".split("/").toList
    val scales  = "-scale=1.2/-scale=1.0/-scale=0.9/-scale=0.8/-scale=0.75/-scale=0.7/-scale=0.6".split("/").toList.reverse
    val screens = "-screen=p/-screen=0/-screen=1/-screen=2".split("/").toList

    lazy val styleSelect: RadioCheckBoxes = RadioCheckBoxes(styles, "-notebook") {
      case None => styleSelect.select(0); style = styles.head
      case Some(i) => style = styles(i)
    }

    lazy val scaleSelect: RadioCheckBoxes = RadioCheckBoxes(scales, "-scale=0.8") {
      case None => scaleSelect.select(0); scale = scales.head
      case Some(i) => scale = scales(i)
    }

    lazy val screenSelect: RadioCheckBoxes = RadioCheckBoxes(screens) {
      case None => screenSelect.select(0); screen = screens.head
      case Some(i) => screen = screens(i)
    }

    import translator._

    val help: Glyph =
    <div width="65em" align="justify">
      <p>
        The START button starts a completely new instance of the GUI.
        The checkboxes determine what tab layout and scale the new instance will have; as well
        as what screen (if there are many) it will be shown on at first.
      </p>
      <p>
         The digits denote the screens offered by the system window manager, and "p"
         denotes the screen preferred by the system.
      </p>
      <fill/>
      <row width="1*width">
        <fill/>
        <div width="0.95*width" textForeground="red" frame="nothing">
          <p hang="-notebook "  parIndent="2em">buttons on the left</p>
          <p hang="-cnotebook"  parIndent="2em">checkboxes on the left</p>
          <p hang="-rnotebook"  parIndent="2em">buttons on the right</p>
          <p hang="-snotebook"  parIndent="2em">buttons slanted along the top</p>
          <p hang="-vnotebook"  parIndent="2em">buttons vertically along the top</p>
          <p hang="-tnotebook"  parIndent="2em">buttons horizontally along the top</p>
        </div>
        <fill/>
      </row>
      <fill/>
      <p align="justify">There is no artificial limit to the number of instances that can be running at once within a single JVM,
        (though space constraints within the JVM will impose a natural limit).</p>
    </div> enlarged 15

    lazy val helpButton: Glyph = TextButton("Help") {
      _ => styled.windowdialogues.Dialogue.OK(help, title="Help").SouthEast(startButton).start()
    }

    lazy val startButton = TextButton(" START ") {
      _ => println(s"$scale $style"); Duplicated.main(Array(scale, style, screen))
    } scaled(2)


    Col.centered(
      startButton, ex scaled 2,
      Row.atTop(
        styleSelect.arrangedVertically(), em scaled 4,
        scaleSelect.arrangedVertically(), em scaled 4,
        screenSelect.arrangedVertically(),
      ),
      ex scaled 2,
      helpButton
    ) enlarged 15
  }
}
