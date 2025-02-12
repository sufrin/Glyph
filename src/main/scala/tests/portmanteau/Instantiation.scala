package org.sufrin.glyph
package tests.portmanteau

import sheeted.{BookSheet, RadioCheckBoxes, TextButton}
import NaturalSize._

import org.sufrin.glyph.Styles.ButtonStyle

class  Instantiation(implicit sheet: BookSheet, implicit val translator: glyphXML.Translation) {
  implicit val  buttons: Sheet = sheet.buttonSheet
  import buttons.{em,ex}

  import GlyphTypes.Window
  val GUI: Glyph = {

    lazy val Duplicated = new Interface with Application {
      def GUI: Glyph =
        if      (extraArgs contains "-notebook")   asRNotebook
        else if (extraArgs contains "-rnotebook")  asRNotebook
        else if (extraArgs contains "-lnotebook")  asLNotebook
        else if (extraArgs contains "-snotebook")  asSNotebook
        else if (extraArgs contains "-vnotebook")  asVNotebook
        else if (extraArgs contains "-tnotebook")  asTNotebook
        else asRNotebook

      def title = s"""Portmanteau -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

      override
      val defaultIconPath: Option[String] = Some("./flag.png")

      override
      def onClose(window: Window): Unit = confirmCloseOn(GUI)(window)

    }

    var style: String = "-notebook"
    var scale: String = "-scale=0.7"
    var screen: String = "-screen=p"
    val styles  = "-notebook/-lnotebook/-snotebook/-vnotebook/-tnotebook".split("/").toList
    val scales  = "-scale=1.2/-scale=1.0/-scale=0.9/-scale=0.8/-scale=0.75/-scale=0.7/-scale=0.6".split("/").toList.reverse
    val screens = "-screen=p/-screen=0/-screen=1/-screen=2".split("/").toList

    lazy val styleSelect: RadioCheckBoxes = RadioCheckBoxes(styles, "-notebook") {
      case None => styleSelect.select(0); style = styles.head
      case Some(i) => style = styles(i)
    }

    lazy val scaleSelect: RadioCheckBoxes = RadioCheckBoxes(scales, "-scale=0.6") {
      case None => scaleSelect.select(0); scale = scales.head
      case Some(i) => scale = scales(i)
    }

    lazy val screenSelect: RadioCheckBoxes = RadioCheckBoxes(screens) {
      case None => screenSelect.select(0); screen = screens.head
      case Some(i) => screen = screens(i)
    }

    import translator._
    Col.centered(
      <div width="55em" align="justify">
        <p>
          The button below starts a completely new instance of the GUI.
          The checkboxes determine what tab layout and scale the new instance will have; as well
          as what screen (if there are many) it will be shown on at first.
        </p>
        <fill/>
        <row inheritwidth="true">
        <fill/>
          <div width="49em" textForeground="red" frame="black">
            <p hang="-notebook "  parIndent="2em">on the right</p>
            <p hang="-lnotebook" parIndent="2em">on the left</p>
            <p hang="-snotebook" parIndent="2em">slanted along the top</p>
            <p hang="-vnotebook" parIndent="2em">vertically along the top</p>
            <p hang="-tnotebook" parIndent="2em">horizontally along the top</p>
          </div>
          <fill/>
        </row>
      </div>,
      ex scaled 2,
      Row(
        TextButton(" New instance ") { _ => println(s"$scale $style"); Duplicated.main(Array(scale, style, screen)) },
      ), ex scaled 2,
      Row.atTop(
        styleSelect.arrangedVertically(), em scaled 4,
        scaleSelect.arrangedVertically(), em scaled 4,
        screenSelect.arrangedVertically(),
      ), ex,
      <p width="55em" align="justify">There is no artificial limit to the number of instances that can be running at once within a single JVM,
        (though space constraints within the JVM will impose a natural limit).</p>
    )
  }
}
