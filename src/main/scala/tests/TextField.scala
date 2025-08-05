package org.sufrin.glyph
package tests

import styled.BookSheet
import unstyled.static
import Brushes.{black, blackFrame, lightGrey, white}

import io.github.humbleui.jwm.Key
import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.glyphXML.Macro
import org.sufrin.glyph.styles.decoration.RoundFramed
import org.sufrin.logging

object TextField extends Application {
  val title: String = "TextField test"

  implicit val style: StyleSheet =
    StyleSheet(textFontSize=25,
               textFontFamily=FontFamily("Menlo"),
               textBackgroundBrush = Brushes.white
               )

  lazy val anchor = static.INVISIBLE()

  val abbreviations = new TextAbbreviations(onLineTrigger = true, implicitUnicode = false, onAmbiguous = TextAbbreviations.ambiguous)
  locally {
    abbreviations.reversible = true
    for {  (abbr, symb) <- StockAbbreviations.all } abbreviations.update(abbr, symb)(SOURCE)
  }

  import glyphXML.Language._
  import styled._
  def SOURCE(implicit source: SourceLocation): String = source.toString()
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
            <col align="left">
              <!--attributes AT="ITEMIZE" /-->
              &BODY;
            </col>
          </span>
        </SCOPE>
        )
  }

  val helpText: Glyph =
    <body width="70em" align="justify" fg="darkGrey" background="white" parSkip="0.75em" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
      <div enlarged="10px" frame="lightgrey.4.sliced(4,4)">
      <p>Interaction is more-or-less standard.</p>
      <itemize itemIndent="2em"  hang="•" >
        <item>The cursor is shown as an I-beam and always kept in view</item>
        <item>The selection has a coloured background</item>
        <item>Visual cues are given when there is out-of-sight text</item>
        <item>The usual cut, copy, paste, and navigate keys are provided:</item>
      </itemize>
      <itemize itemIndent="2em" hang="•">
        <item>Home, End, Left, Right, and Backspace - usual effect</item>
        <item>Ctrl/Cmd Backspace - swaps the two characters before the cursor </item>
        <item>Ctrl/Cmd C - copy the selection (default all) to clipboard</item>
        <item>Ctrl/Cmd X - cut the selection (default all) to clipboard</item>
        <item>Ctrl/Cmd V - insert from clipboard</item>
        <item>Mousebutton - set the cursor here</item>
        <item>The selection is between the mark (if any) and the cursor)</item>
        <itemize itemIndent="4em" hang="•">
          <item>Secondary mousebutton - set the mark here</item>
          <item>Ctrl/Cmd mousebutton - set the mark here</item>
          <item>Ctrl/Cmd . - set the mark at the cursor</item>
          <item>Ctrl/Cmd S - swap the mark and cursor</item>
        </itemize>
      </itemize>
      </div>
    </body>

  val defs = translation.meaning
  val frameGrey = Brushes.darkGrey(width=2)

  val hintSheet   = style.copy(fontScale=0.6f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val buttonSheet = style.copy(fontScale=0.8f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val controlsStyle: StyleSheet =
    buttonSheet//style.copy(fontScale=0.85f, buttonDecoration = RoundFramed(fg=Brushes.darkGrey(width=4), bg=Brushes.lightGrey, enlarge=0.3f, radius=10))

  val liveSubstitution: ToggleVariable = ToggleVariable(abbreviations.onLineTrigger)   { state => abbreviations.onLineTrigger = state }
  val implicitUnicode: ToggleVariable  = ToggleVariable(abbreviations.implicitUnicode) { state => abbreviations.implicitUnicode=state }

  def triggerButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable automatic substitution for abbreviations\nas they are typed.\n(SHIFT-SHIFT substitutes when disabled)")(hintSheet))(liveSubstitution)(buttonSheet)

  def implicitButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable implicit abbreviations\nof unicode glyphs\nexpressed as hex digit sequences\n followed by \"uu\" ")(hintSheet))(implicitUnicode)(buttonSheet)


  defs("CONTROLS") =
    _=>
      NaturalSize.Row(align=Mid)(
        Label("Live substitution: ")(controlsStyle), triggerButton,
        Label("  Implicit unicode: ")(controlsStyle), implicitButton
      )


  val textField: TextField =
    styled.TextField(size = 40,
                     onEnter = showDimensions,
                     onError = {
                       case (eventKey, _) =>
                         import Modifiers._
                         logging.SourceDefault.warn(s"Undefined keystroke: ${eventKey.asShortString}")},
                     abbreviations = abbreviations,
                     onNewGlyph = styled.TextField.reportNewGlyph(_,_),
                     initialText = "")(style.copy(fontScale = 1.1f)).withAbbreviationKey(Key.ESCAPE)

  defs("TEXTFIELD") = _=>textField.framed()

  lazy val helpButton: Glyph = styled.TextButton("Help"){
    _ => windowdialogues.Dialogue.FLASH(helpText.enlarged(20)).South(helpButton).start()
  }(controlsStyle)

  defs("HELPBUTTON") = _ => helpButton

  defs("SHOWBUTTON") = _ =>  styled.TextButton("Show", hint=Hint(5, "Show all the abbreviations")(hintSheet)){
    _ =>
      val order = new Ordering[String] { def compare(x: String, y: String): Int = x.compareToIgnoreCase(y) }
      val substitute =
        StockAbbreviations.all.toMap
      val pairs =
        StockAbbreviations.all.map(_._1).toSeq.sorted(order).
          flatMap{ key => List(Label(key), Label(substitute(key)))}
      val content =
        NaturalSize.Grid(fg=black, bg=white, padx=10, pady=5).table(width=28)(pairs)
      windowdialogues.Dialogue.FLASH(content.enlarged(20)).OnRootOf(anchor, Vec(50,50)).start()
  }(controlsStyle)

  defs("UNICODE") = _ => styled.TextButton("Unicode", hint=Hint(5, "Replace glyph at cursor left\nby its unicode codepoint(s)")(hintSheet)) {
    _ => textField.unicode()
  }(controlsStyle)

  defs("UNABBR") = _ => styled.TextButton("Substitution", hint=Hint(5, "Replace substitution at cursor\nby its abbreviation\n(inverse of 'abbreviate')")(hintSheet)) {
    _ => textField.unabbreviation()
  }(controlsStyle)

  def showDimensions(text: String): Unit = {
    //val t=unstyled.Text(text, style.textFont)
    //println(s"$text(${t.w},${t.h}) height=${t.height} a=${t.ascent}, desc=${t.descent} l=${t.leading} s=${t.spacing} drop=${t.drop}")
  }


  lazy val GUI: Glyph = NaturalSize.Col(align=Center, bg=Brushes.white)(
    <body width={s"${20+textField.w}px"} align="center"  parskip="2ex" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
      <p><glyph gid="TEXTFIELD"/></p>
      <p>
        <glyph gid="CONTROLS"/>
        <glyph gid="HELPBUTTON"/>
        <glyph gid="SHOWBUTTON"/>
        <glyph gid="UNABBR"/>
        <glyph gid="UNICODE"/>
      </p>
    </body>
  ).enlargedBy(10, 50)
}


