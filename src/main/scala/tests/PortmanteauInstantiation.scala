package org.sufrin.glyph
package tests

import sheeted.{RadioCheckBoxes, TextButton}
import NaturalSize._

class  PortmanteauInstantiation(implicit sheet: Sheet, implicit val translator: glyphXML.Translation) {
  import GlyphTypes.Window
  val GUI: Glyph = {

    lazy val Duplicated = new PortmanteauInterface with Application {
      lazy val GUI: Glyph =
        if      (extraArgs contains "-notebook")   asRNotebook
        else if (extraArgs contains "-rnotebook")  asRNotebook
        else if (extraArgs contains "-lnotebook")  asLNotebook
        else if (extraArgs contains "-snotebook")  asSNotebook
        else if (extraArgs contains "-vnotebook")  asVNotebook
        else if (extraArgs contains "-tnotebook")  asTNotebook
        else if (extraArgs contains "-menu")       asMenu
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
    val styles  = "-notebook/-lnotebook/-snotebook/-vnotebook/-tnotebook/-menu".split("/").toList
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

    val em = sheet.em
    val ex = sheet.ex
    import translator._
    Col.centered(
      <p width="30em" align="justify">
        The button below starts a completely new instance of the GUI.
        The checkboxes determine what tab layout and scale the new instance will have; as well
        as what screen (if there are many) it will be shown on at first.
        There is no artificial limit to the number of instances that can be running at once within a single JVM,
        (though space constraints within the JVM will impose a natural limit).
      </p>,
      ex scaled 2,
      Row(
        TextButton(" New instance ") { _ => println(s"$scale $style"); Duplicated.main(Array(scale, style, screen)) },
      ), ex scaled 2,
      Row.atTop(
        styleSelect.arrangedVertically(), em scaled 4,
        scaleSelect.arrangedVertically(), em scaled 4,
        screenSelect.arrangedVertically(),
      ), ex
    )
  }
}
