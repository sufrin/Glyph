package org.sufrin.glyph
package tests
import GlyphTypes.Window
import sheeted.{Label, CheckBox, Notebook}
import NaturalSize.{Col, Row}

class PortmanteauInterface(implicit val style: Sheet, implicit val translator: glyphXML.Translation) extends Notebook {

  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import sheeted.overlaydialogues.Dialogue.OKNO
    // TODO: windowdialogues need set software scale more carefully than now if autoScale
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
      Label("Do you want to Exit?") scaled 1.5f
    ).enlarged(50)
    OKNO(prompt,
      title = "Exit Dialogue", ok = " Exit now ", no = " Continue ").InFront(glyph).andThen(close => if (close) window.close())
  }


  locally {
    translator("anchor") = { _ => Glyphs.INVISIBLE() }
  }

  Page("Welcome", "") {
    val anchor = Glyphs.INVISIBLE()
    val checkBox = CheckBox(initially=false) { state => anchor.guiRoot.autoScale=state }(style.copy(buttonFrame = Styles.Decoration.Framed(fg=DefaultBrushes.blue, bg=DefaultBrushes.nothing)))
    import translator._
    translator("anchor")   = { _ => anchor }
    translator("checkbox") = { _ => checkBox }
    <body width="30em" background="nothing">
      <p align="justify">
        Welcome to the Portmanteau Notebook: its source code is more
        modular than that of DemonstrationNotebook -- which evolved as
        a monolith.
      </p>
      <glyph gid="anchor"/>
      <row inheritwidth="true">
         <fill/><span>Enable window resizing: </span><glyph gid="checkbox"/><fill/>
      </row>
    </body>
  }

  Page("New Instance", "")(new PortmanteauInstantiation().GUI)

  /*
  Page("Transforms*", "")(new PortmanteauTransforms().Layout.leftButtons())

  Page("Text", "") (new PortmanteauText().GUI)

  Page("GlyphML", "") (new PortmanteauGlyphML().GUI)
  */

  import utils.Output.withWriteBar

  lazy val asMenu      = withWriteBar()(Layout.menuBar)
  lazy val asRNotebook = withWriteBar()(Layout.rightButtons())
  lazy val asLNotebook = withWriteBar()(Layout.leftButtons())
  lazy val asVNotebook = withWriteBar()(Layout.rotatedButtons(3))
  lazy val asSNotebook = withWriteBar()(Layout.skewedButtons(0.2f, 0f, uniform = true))
  lazy val asTNotebook = withWriteBar()(Layout.topButtons())
}
