package org.sufrin.glyph
package tests.GlyphBook


import styled._
import unstyled.static

import io.github.humbleui.jwm.Key
import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.tests.StockAbbreviations
import org.sufrin.glyph.Brushes.{black, blackFrame, colourName, lightGrey, red, white}
import org.sufrin.logging


class TextTool(implicit style: StyleSheet)  {
  implicit val pageStyle: BookSheet = BookSheet(style, style)
  val anchor = static.INVISIBLE()
  val abbreviations = new TextAbbreviations(onLineTrigger = true, implicitUnicode = false, onAmbiguous = TextAbbreviations.ambiguous)

  locally {
    abbreviations.reversible = true
    for {  (abbr, symb) <- StockAbbreviations.all } abbreviations.update(abbr, symb)(SOURCE)
  }

  val controlsStyle: StyleSheet =
      pageStyle.pageSheet.copy(labelFontSize = 30, buttonFontSize = 30, buttonDecoration = styles.decoration.Shaded(fg=blackFrame, enlarge=0.2f))


  import glyphXML.Language._
  import styled._
  def SOURCE(implicit source: SourceLocation): String = source.toString()

  val defs = translation.meaning

  defs("CONTROLS") =
    _=> //Label("Log events") beside styled.CheckBox(initially=false) { state => anchor.guiRoot.eventHandler.logEvents=state } beside
      NaturalSize.Row(align=Mid)(
        Label(" LIVE(")(controlsStyle),
        CheckBox(initially=abbreviations.onLineTrigger) {
           state => abbreviations.onLineTrigger=state
        }(controlsStyle),
        Label(") UU(")(controlsStyle),
        CheckBox(initially=abbreviations.implicitUnicode) {
          state => abbreviations.implicitUnicode=state
    }(controlsStyle) beside  Label(")")(controlsStyle)
      )


  val textField: TextField =
      styled.TextField(size = 40,
                       onEnter = { _ =>  },
                       onCursorLeave = { _ => anchor.guiRoot.giveupFocus() },
                       onError = {
                         case (eventKey, _) =>
                           import Modifiers._
                           logging.SourceDefault.warn(s"Undefined keystroke: ${eventKey.asShortString}")},
                       abbreviations = abbreviations,
                       onNewGlyph = TextField.reportNewGlyph(_,_),
                       initialText = "Æsop’s Φabulous \uD83C\uDF08 φαβλ")(style.copy(fontScale = 1.5f)).withAbbreviationKey(Key.ESCAPE)
  defs("TEXTFIELD") = _=>textField.framed()


  val helpText: Glyph =
    <body width="70em" align="justify" fg="blue" parSkip="0.75em" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
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
  </body>



  defs("HELPBUTTON") = _ =>  styled.TextButton("Help"){
    _ => windowdialogues.Dialogue.FLASH(helpText.enlarged(20, bg=lightGrey)).InFront(anchor).start()
  }(controlsStyle)

  defs("SHOWBUTTON") = _ =>  styled.TextButton("Show", hint=Hint(5, "Show all the abbreviations")){
    _ =>
      val order = new Ordering[String] { def compare(x: String, y: String): Int = x.compareToIgnoreCase(y) }
      val substitute =
        StockAbbreviations.all.toMap
      val pairs =
        StockAbbreviations.all.map(_._1).toSeq.sorted(order).
        flatMap{ key => List(Label(key), Label(substitute(key)))}
      val content =
        NaturalSize.Grid(fg=black, bg=white, padx=10, pady=5).table(width=28)(pairs)
      windowdialogues.Dialogue.FLASH(content.enlarged(20)).InFront(anchor).start()
  }(controlsStyle)

  defs("UNICODE") = _ => styled.TextButton("Unicode", hint=Hint(5, "Replace glyph at cursor left\nby unicode codepoint(s)")) {
    _ => textField.unicode()
  }(controlsStyle)

  defs("UNABBR") = _ => styled.TextButton("Abbr", hint=Hint(5, "Replace cursor left material\nby its abbreviation\n(inverse of 'abbreviate')")) {
    _ => textField.unabbreviation()
  }(controlsStyle)


  val GUI: Glyph = NaturalSize.Col(align=Center)(
    anchor,
    <body width="70em" align="justify" fg="blue" parSkip="0.75em" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
      <p>
        This is an example of a TextField that has been set up by mapping some abbreviations (see <tt>StockAbbreviations.scala</tt>) to symbols or pictographs, for example:
      </p>
      <fill/>
      <p align="center" bg="transparent" fontFamily="Courier" source={SOURCE}>
        <![CDATA[(c) (r) :) :O <3 :-| aleph. Alpha. rainbow=🌈 ukflag=🇬🇧 ae=ᴂ Ae=Æ]]>
      </p>
      <fill/>
      <p align="center"><glyph gid="TEXTFIELD"/></p>
      <fill/>
      <p align="center">
        <glyph gid="CONTROLS"/>
        <glyph gid="HELPBUTTON"/>
        <glyph gid="SHOWBUTTON"/>
        <glyph gid="UNABBR"/>
        <glyph gid="UNICODE"/>
      </p>
      <fill/>
      <p hang="Abbreviations: ">
        When "LIVE" is checked, typing the last character of an abbreviation results in the insertion of the
        sequence or symbol it abbreviates, otherwise typing the SHIFT key twice in succession when the cursor is at the end of
        an abbreviation has the same result, as (here) does the ESC key. If "UU" is checked then sequences of the form <i>hex digits</i><b>uu</b> are
        implicitly taken as abbreviations for the corresponding unicode character.
      </p>
      <fill/>
      <p>
        <b>Practice (with live abbreviations enabled): </b>type <i>help</i>; pause, then the SHIFT key twice in succession. Type <i>tickgreen</i>,
        amd see what happens if you paste in (singly) pictograms.
      </p>
      <fill/>
      </body>
  )
}


