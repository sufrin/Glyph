package org.sufrin.glyph
package tests.demonstrationBook

import styled.{BookSheet, Label, RadioCheckBoxes, TextButton}
import NaturalSize._
import styles.decoration

class  SplashScreen(implicit val sheet: BookSheet, implicit val translator: glyphXML.Translation) {
  implicit val  buttons: StyleSheet =
                sheet.pageSheet.copy(
                  buttonDecoration =
                    decoration.Edged(sheet.pageSheet.buttonForegroundBrush(width=3).sliced(5,2),
                                     sheet.pageSheet.buttonBackgroundBrush)
                )
  import buttons.{em,ex}

  import GlyphTypes.Window


  lazy val thisApplication: Application = new Interface with Application {

      // The computed root ofPaint the running glyph,
      private var theRootGlyph:Glyph = null

      def GUI: Glyph = {
        theRootGlyph =
        if      (extraArgs contains "-notebook")   asLNotebook
        else if (extraArgs contains "-rnotebook")  asRNotebook
        else if (extraArgs contains "-lnotebook")  asLNotebook
        else if (extraArgs contains "-snotebook")  asSNotebook
        else if (extraArgs contains "-vnotebook")  asVNotebook
        else if (extraArgs contains "-tnotebook")  asTNotebook
        else if (extraArgs contains "-cnotebook")  asCheckBoxes
        else asLNotebook
        theRootGlyph
      }

      def title = s"""Demonstration Book -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

      override
      val defaultIconPath: Option[String] = Some("./flag.png")

      override
      def handleWindowCloseRequest(window: Window): Unit = confirmCloseOn(theRootGlyph)(window)

      def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
        import styled.windowdialogues.Dialogue.OKNO
        // TODO: windowdialogues need set software scale more carefully than now if autoScale
        val prompt = Row(align=Mid)(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
          Label("Do you want to Exit?")(sheet.pageSheet) scaled 1.5f
        ).enlarged(50)
        OKNO(prompt,
          title = "Exit Dialogue", ok = " Exit now ", no = " Continue ")(button).InFront(glyph).andThen(close => if (close) window.close())
      }
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
    </div>.enlarged(15).framed(DefaultBrushes.black.sliced(10, 5)).enlarged(15)


    val splashStyle: StyleSheet = sheet.buttonSheet.copy(
        fontScale=1.1f
    )

    lazy val startButton = TextButton(" START ") {
      _ => thisApplication.main(Array(scale, style, screen))
    }(splashStyle)

    lazy val helpButton: Glyph = TextButton("Help") {
      _ => styled.windowdialogues.Dialogue.OK(help, title="Help").SouthEast(helpButton).start()
    }(splashStyle)


    val styleDefinitionButtons: Glyph = Row(align=Top)(
      styleSelect.arrangedVertically(), em scaled 4,
      scaleSelect.arrangedVertically(), em scaled 4,
      screenSelect.arrangedVertically(),
    ) . enlarged(15) . edged(splashStyle.buttonForegroundBrush(width=3).sliced(5, 2))

    val GUI: Glyph = Col(align=Left)(
      FixedSize.Row(align=Top, width = styleDefinitionButtons.w*1.1f)(startButton, splashStyle.hFill(), helpButton),
      ex scaled 2,
      styleDefinitionButtons,
    ) enlarged 15

}
