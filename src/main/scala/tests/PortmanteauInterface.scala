package org.sufrin.glyph
package tests
import GlyphTypes.Window
import sheeted.{Book, BookSheet, CheckBox, Label}
import NaturalSize.{Col, Row}

class PortmanteauInterface(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
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


  locally {
    translation("anchor") = { _ => Glyphs.INVISIBLE() }
  }

  Page("Welcome", "") {
    val anchor = Glyphs.INVISIBLE()
    val checkBox = CheckBox(initially=false) { state => anchor.guiRoot.autoScale=state }(content.copy(buttonFrame = Styles.Decoration.Framed(fg=DefaultBrushes.blue, bg=DefaultBrushes.nothing)))
    import translation._
    translation("anchor")   = { _ => anchor }
    translation("checkbox") = { _ => checkBox }
    <body width="40em">
      <p align="justify">
        Welcome to the Portmanteau Notebook: its source code is more
        modular than that of DemonstrationNotebook -- which evolved as
        a monolith.
      </p>
      <glyph gid="anchor"/>
      <fill/>
      <row inheritwidth="true" background="nothing">
         <fill/><span>Enable window resizing:</span><glyph gid="checkbox"/><fill/>
      </row>
    </body>
  }

  Page("New Instance", "")(new PortmanteauInstantiation().GUI)

  Page("Text", "") (new PortmanteauText().GUI)


  /*
  Page("Transforms*", "")(new PortmanteauTransforms().Layout.leftButtons())


  Page("GlyphML", "") (new PortmanteauGlyphML().GUI)
  */

  import utils.Output.withWriteBar
  import book.Layout

  lazy val asRNotebook = withWriteBar()(Layout.rightButtons())
  lazy val asLNotebook = withWriteBar()(Layout.leftButtons())
  lazy val asVNotebook = withWriteBar()(Layout.rotatedButtons(3))
  lazy val asSNotebook = withWriteBar()(Layout.skewedButtons(0.2f, 0f, uniform = true))
  lazy val asTNotebook = withWriteBar()(Layout.topButtons())
}
