package org.sufrin.glyph
package tests.GlyphBook
import styled.{Book, BookSheet, CheckBox, ToggleVariable}
import NaturalSize.Col
import glyphXML.Macro
import styles.decoration
import unstyled.BooleanGlyphs.OnOffButton
import unstyled.static

class Interface(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  val book = Book()
  val Page = book.Page
  implicit val content: StyleSheet = style.pageSheet
  val button: StyleSheet = style.buttonSheet

  var enableSave: Boolean = false
  val saveEnable = ToggleVariable(initially=enableSave){
    state => enableSave = state
  }

  /** Checkbox indicating whether the write bar is enabled. */
  val enableSaveCheckBox =
    CheckBox(initially=saveEnable.value) (saveEnable)


  locally {
    translation("anchor") = { _ => static.INVISIBLE() }
    translation("caption") =
      new Macro(<p align="center"><b>&BODY;</b></p>)

    /*
     *  A simple implementation of <itemize> blocks containing <item>s.
     *  {{{
     *    <itemize logging[=false]
     *             leftMargin[=5em]
     *             hang[=" * "]
     *             itemIndent[=2em]
     *             itemAlign[=justify]>
     *
     *            <item>...<item>
     *            <item>...<item>
     *              ...
     *            <item>...<item>
     *
     *    </itemize>
     *  }}
     *
     *  Each <item> can specify its own hang, itemAlign, and itemWidth attributes, but otherwise inherits them from
     *  the closest lexically enclosing <itemize>
     *
     *  <itemize> environments may not (at present) be nested, but the appearance of
     *  nesting can be given by changing hang text and increasing the itemIndent.
     */

    translation("item") =
      new Macro(
        <row inheritwidth="true">
          <!--attributes AT="ITEM" id="tag:item"/-->
          <fill width="$itemindent"/>
          <p hang="$hang" width="$itemwidth" align="$itemalign">
            &BODY;
          </p>
        </row>)

    translation("itemize") =
      new Macro(
        <SCOPE>
        <ATTRIBUTES key="tag:item" logging="$logging(false)" leftmargin="$leftmargin(5em)" hang="$hang( * )"  itemindent="$itemindent(2em)"  itemwidth="$itemwidth(70em)" itemalign="$itemalign(justify)"/>
        <span itemindent="$itemindent(2em)">
          <col align="left" >
            <!--attributes AT="ITEMIZE" /-->
            &BODY;
          </col>
        </span>
        </SCOPE>
      )
  }

  Page("Welcome", "") {
    import glyphXML.Language._
    val anchor = static.INVISIBLE()
    Col(align=Center)(
    <body align="justify" width="65em">
      <p>
        This application demonstrates aspects of the Glyphs library
        by offering the choice of several demonstration interfaces. These are shown on
        the pages of a "tabbed" notebook and the location and style of
        tabs is determined when the main application is started from the <b>Splash Screen</b>.
        Several of the interfaces have nested  interfaces within them:
        their names have * by them.
      </p>
      <p>
        The notebook style is initially -notebook, and
        its scale is initially 1.00. These can  be changed when creating a
        new instance from the <b>Splash Screen</b>; and the scale can also be changed
        when the window is resized by dragging an edge/corner.
      </p>
    </body>, anchor
    ).enlarged(20)
  }

  Page("Window Menu Support*", "") (new WindowMenus().GUI)

  Page("Button styles*", "") (new ButtonStyles().GUI)

  Page("Glyph Framing*", "") (new Framing().GUI)

  Page("Using Overlays*", "") (new OverlayUses().GUI)

  Page("Text Tool", "") (new TextTool().GUI)

  Page("Fonts", "Font families\n(available on this computer)\n\n\n") {
    val chooser = new FontAndBrushChooser()
    import glyphXML.Language._
    import content.ex
    chooser.fontChooser.showExample()
    Col(align=Center)(
      <div width="75em" align="justify">
        <p>
          This page draws the example text with the selected brush, using the chosen font, style, and size.
          Notice the difference in appearance between fill-mode and stroke-mode brushes. In stroke mode
          the text appears "outlined", especially when the brush width is relatively small; and this
          appearance persists when the font size is larger, even when the brush width is quite large.
        </p>
      </div>, ex, ex,
      chooser.GUI
      ).enlarged(20)
  }

  Page("Events/Windows*", "") (new EventsAndWindows().GUI)

  Page("Glyph Transforms*", "") (new Transforms().GUI)

  Page("Animation*", "")(new Animation().GUI)

  Page("Etc*", "") (new Etcetera().GUI)

  Page("Splash Screen", "")(new SplashScreen().GUI)


  import utils.Output.withWriteBar

  import book.Layout
  import translation._


  val hint: Glyph =
    <p width="55em" align="justify" fontScale="0.7" frame="red.2">
    clicking on this grey strip invites you to save the GUI's
    current appearance in a .png file.
    </p>

  val writeBarStyle: StyleSheet = content.copy(fontScale=0.9f, buttonDecoration = decoration.Framed())


  lazy val asCheckBoxes = withWriteBar(hint=hint, enabled = true)(Layout.leftCheckBoxes(pageAlign = Center))(writeBarStyle)
  lazy val asRNotebook = withWriteBar(hint=hint, enabled = true)(Layout.rightButtons(pageAlign = Center))(writeBarStyle)
  lazy val asLNotebook = withWriteBar(hint=hint, enabled = true)(Layout.leftButtons(pageAlign = Center))(writeBarStyle)
  lazy val asVNotebook = withWriteBar(hint=hint, enabled = true)(Layout.rotatedButtons(3, pageAlign = Center))(writeBarStyle)
  lazy val asSNotebook = withWriteBar(hint=hint, enabled = true)(Layout.skewedButtons(0.2f, 0f, buttonAlign = Justify, pageAlign = Center))(writeBarStyle)
  lazy val asTNotebook = withWriteBar(hint=hint, enabled = true)(Layout.topButtons(pageAlign = Center))(writeBarStyle)
  lazy val asMenu      = NaturalSize.Col(align=Left)(Layout.popupMenu(style, withWriteBar(hint=hint, enabled = true)(_)(writeBarStyle)))
}
