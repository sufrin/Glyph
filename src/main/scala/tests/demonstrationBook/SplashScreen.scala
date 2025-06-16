package org.sufrin.glyph
package tests.demonstrationBook

import styled.{BookSheet, Label, RadioCheckBoxes, TextButton}
import NaturalSize._
import styles.decoration
import Location.South

class  SplashScreen(implicit val sheet: BookSheet, implicit val translator: glyphXML.Translation) {
  implicit val  buttons: StyleSheet =
                sheet.pageSheet.copy(
                  buttonDecoration =
                    decoration.Edged(sheet.pageSheet.buttonForegroundBrush(width=3).sliced(5,2),
                                     sheet.pageSheet.buttonBackgroundBrush)
                )
  import GlyphTypes.Window

  import buttons.em


  lazy val thisApplication: Application = new Interface with Application {

      // The computed root of the running glyph,
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
        else if (extraArgs contains "-menu")       asMenu
        else asLNotebook
        theRootGlyph
      }

      def title = s"""Sampler -scale=$scaleFactor ${extraArgs.mkString(", ")}"""

      override
      val defaultIconPath: Option[String] = Some("./flag.png")

      override def whenStarted(): Unit = {
        super.whenStarted()
        Application.confirmCloseRequestsFor(GUI)(sheet.pageSheet)
        Application.enableAutoScaleFor(GUI)
      }

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
    val styles  = "-notebook/-cnotebook/-rnotebook/-snotebook/-vnotebook/-tnotebook/-menu".split("/").toList
    val scales  = "-scale=1.2/-scale=1.0/-scale=0.9/-scale=0.8/-scale=0.75/-scale=0.7/-scale=0.6".split("/").toList.reverse
    val screens = "-screen=p/-screen=0/-screen=1/-screen=2".split("/").toList

    val splashStyle: StyleSheet = sheet.buttonSheet.copy(
      fontScale=3f,
      buttonBackgroundBrush=Brushes.blue,
      buttonForegroundBrush=Brushes.white,
      buttonDecoration = decoration.RoundFramed(fg=Brushes.blue(width=10), bg=Brushes.blue, radius = 40)
    )

    val selectButtonStyle: StyleSheet = sheet.buttonSheet.copy(
      buttonDecoration = decoration.Framed(fg=Brushes.blue)
    )

    lazy val styleSelect: RadioCheckBoxes = RadioCheckBoxes(styles, "-notebook") {
      case None => styleSelect.select(0); style = styles.head
      case Some(i) => style = styles(i)
    }(selectButtonStyle)

    lazy val scaleSelect: RadioCheckBoxes = RadioCheckBoxes(scales, "-scale=0.8") {
      case None => scaleSelect.select(0); scale = scales.head
      case Some(i) => scale = scales(i)
    }(selectButtonStyle)

    lazy val screenSelect: RadioCheckBoxes = RadioCheckBoxes(screens) {
      case None => screenSelect.select(0); screen = screens.head
      case Some(i) => screen = screens(i)
    }(selectButtonStyle)

    import translator._

    val help: Glyph =
    <div width="65em" parSkip="1ex" align="justify" labelBackground="lightGrey" background="lightGrey" textBackground="lightGrey" textForeground="black">
      <SCOPE>
        <MACRO key="strut"><fill width="1*width" bg="lightGrey"/></MACRO>
        <MACRO key="centered"><row width="1*width"><fill/>&BODY;<fill/></row></MACRO>
        <p>
        The <b>Glyph Sampler</b> button starts a completely new instance of the GUI.
        The checkboxes below it determine what tab layout and scale the new instance will have; as well
        as what screen (if there are many) it will be shown on at first.
      </p>
      <p>
         The digits denote the screens offered by the system window manager, and <tt textForeground="blue">"p"</tt>
         denotes the screen preferred by the system.
      </p>
      <p>All pages will change size and rescale themselves if their window is resized.</p>
      <strut/>
      <centered>
        <div width="0.95*width" parSkip="0ex" parIndent="2em">
          <p hang="-notebook ">buttons on the left</p>
          <p hang="-cnotebook">checkboxes on the left</p>
          <p hang="-rnotebook">buttons on the right</p>
          <p hang="-snotebook">buttons slanted along the top</p>
          <p hang="-vnotebook">buttons vertically along the top</p>
          <p hang="-tnotebook">buttons horizontally along the top</p>
          <p hang="-menu     ">individual pages are on a menu</p>
        </div>
      </centered>
      <strut/>
      <p align="justify">
        There is no artificial limit to the number of instances that can be running at once within a single JVM,
        (though space constraints within the JVM will impose a natural limit).
      </p>
      </SCOPE>
    </div>.enlarged(20).framed(Brushes.black.sliced(10, 5)).enlarged(25)




    lazy val startButton = TextButton(" START ") {
      _ => thisApplication.main(Array(scale, style, screen))
    }(splashStyle)

    lazy val helpButton: Glyph = TextButton("Help") {
      _ => styled.windowdialogues.Dialogue.BORDERLESS((help), South(helpButton)).start()
    }(splashStyle.copy(fontScale=1))

    lazy val debug: Glyph = styled.CheckBox(initially=false){
      state => debug.guiRoot.eventHandler.logEvents = state
    }


    val styleDefinitionButtons: Glyph = Row(align=Top)(
      styleSelect.arrangedVertically(), em scaled 4,
      scaleSelect.arrangedVertically(), em scaled 4,
      screenSelect.arrangedVertically(),
    ) . enlarged(15) . edged(Brushes.blue)

    val GUI: Glyph = Col(align=Center)(
      TextButton(" Glyph Sampler ") { _ => thisApplication.main(Array(scale, style, screen))} (splashStyle.copy(fontScale=3f)),
      splashStyle.vFill(),
      styleDefinitionButtons,
      splashStyle.vFill(),
      FixedSize.Row(align=Top, width = styleDefinitionButtons.w)(splashStyle.hFill(), helpButton),
    ) enlarged 15

}
