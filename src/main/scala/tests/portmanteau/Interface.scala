package org.sufrin.glyph
package tests.portmanteau
import GlyphTypes.Window
import sheeted.{Book, BookSheet, CheckBox, Label}
import NaturalSize.{Col, Row}
import BooleanGlyphs.OnOffButton

import org.sufrin.glyph.Styles.Decoration

class Interface(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  val book = Book()
  val Page = book.Page
  implicit val content: Sheet = style.pageSheet
  val button: Sheet = style.buttonSheet

  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import sheeted.overlaydialogues.Dialogue.OKNO
    // TODO: windowdialogues need set software scale more carefully than now if autoScale
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
      Label("Do you want to Exit?")(content) scaled 1.5f
    ).enlarged(50)
    OKNO(prompt,
      title = "Exit Dialogue", ok = " Exit now ", no = " Continue ")(button).InFront(glyph).andThen(close => if (close) window.close())
  }

  var enableSave: Boolean = false
  val saveEnable = BooleanVariable[OnOffButton](initially=enableSave){
    state => enableSave = state
  }

  /** Checkbox indicating whether the write bar is enabled. */
  val enableSaveCheckBox: OnOffButton =
    CheckBox(initially=saveEnable.value) (saveEnable)


  locally {
    translation("anchor") = { _ => Glyphs.INVISIBLE() }
  }

  Page("Welcome", "") {
    val anchor = Glyphs.INVISIBLE()
    val checkBox = CheckBox(initially=false) { state => anchor.guiRoot.autoScale=state }(content.copy(buttonFrame = Styles.Decoration.Framed(fg=DefaultBrushes.blue, bg=DefaultBrushes.nothing)))
    import translation._
    translation("checkbox") = { _ => checkBox }
    Col.centered(
    <body align="justify" width="50em">
      <p>
        This application demonstrates aspects of the Glyphs library
        by offering the choice of several demonstration GUIs. These are shown on
        the pages of a tabbed notebook with the location and style of
        tabs determined by the command line.
        Several of the pages have pages nested  within them:
        their names have * by them.
      </p>
      <p>
        The notebook style is initially -notebook, and
        its scale is initially 1.00. These can  be changed when creating a
        new instance from the <tt>New Instance</tt> page; and the scale is also changed
        when the window is resized by dragging an edge/corner when <glyph gid="checkbox"/> is checked.
      </p>
    </body>, anchor
    )
  }

  Page("New Instance", "")(new Instantiation().GUI)

  Page("Text", "") (new TextTool().GUI)

  Page("Window Menu Support*", "") (new WindowMenus().GUI)

  Page("Glyph Transforms*", "") (new Transforms().GUI)

  Page("Button Styles*", "") (new ButtonStyles().GUI)

  Page("Framing", "") (new Framing().GUI)

  Page("Using Overlays*", "") (new OverlayUses().GUI)

  Page("Events/Windows", "") (new EventsAndWindows().GUI)

  Page("Etc*", "") (new Etcetera().GUI)





  import utils.Output.withWriteBar
  import book.Layout
  import translation._


  val hint: Glyph =
    <p width="55" align="justify" fontScale="0.7" frame="red/2">
    clicking on this grey strip invites you to save the GUI's
    current appearance in a .png file.
    </p>

  val writeBarStyle: Sheet = content.copy(fontScale=0.9f, buttonFrame = Decoration.Framed())
  lazy val asRNotebook = withWriteBar(hint=hint, enabled = true)(Layout.rightButtons())(writeBarStyle)
  lazy val asLNotebook = withWriteBar(hint=hint, enabled = true)(Layout.leftButtons())(writeBarStyle)
  lazy val asVNotebook = withWriteBar(hint=hint, enabled = true)(Layout.rotatedButtons(3))(writeBarStyle)
  lazy val asSNotebook = withWriteBar(hint=hint, enabled = true)(Layout.skewedButtons(0.2f, 0f, uniform = true))(writeBarStyle)
  lazy val asTNotebook = withWriteBar(hint=hint, enabled = true)(Layout.topButtons())(writeBarStyle)
}
