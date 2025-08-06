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
        Label(" Live substitution(")(controlsStyle),
        CheckBox(initially=abbreviations.onLineTrigger) {
           state => abbreviations.onLineTrigger=state
        }(controlsStyle),
        Label(") Implicit Unicode(")(controlsStyle),
        CheckBox(initially=abbreviations.implicitUnicode) {
          state => abbreviations.implicitUnicode=state
    }(controlsStyle) beside  Label(")")(controlsStyle)
      )


  val textField: TextField =
      styled.TextField(size = 20,
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


  val helpText: Glyph = TextField.helpText(style)


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

  defs("UNICODE") = _ => styled.TextButton("Unicode", hint=Hint(5, "Replace glyph at left of cursor\nby its unicode codepoint(s)")) {
    _ => textField.unicode()
  }(controlsStyle)

  defs("UNABBR") = _ => styled.TextButton("Substitution", hint=Hint(5, "Replace substitution at left of cursor\nby its abbreviation")) {
    _ => textField.unabbreviation()
  }(controlsStyle)


  val GUI: Glyph = NaturalSize.Col(align=Center)(
    anchor,
    <body width="70em" align="left" fg="blue" parSkip="0.75em" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
      <p>
        This is an example of a TextField that has been set up by map_ping abbrev_iations (see <tt>StockAbbreviations.scala</tt>) to sym_bols or pict_ographs, for example:
      </p>
      <fill/>
      <p align="center" bg="transparent" fontFamily="Courier" source={SOURCE}>
        <![CDATA[(c) (r) :) :O <3 :-| alef/ alpha/ rainbow=🌈 ukflag=🇬🇧 ae=ᴂ Ae=Æ]]>
      </p>
      <fill/>
      <p align="center"><glyph gid="TEXTFIELD"/></p>
      <fill/>
      <p align="center">
        <glyph gid="CONTROLS"/>
      </p>
      <p align="center">
        <glyph gid="SHOWBUTTON"/>
        <glyph gid="UNABBR"/>
        <glyph gid="UNICODE"/>
        <glyph gid="HELPBUTTON"/>
      </p>
      <fill/>
      <p hang="Abbreviations: ">
        When "Live..." is checked, typing the last character of an abbrev_iation results in the insertion of the
        sequence or symbol it abbrev_iates, otherwise typing the SHIFT key twice in succ_ession when the cursor is at the end of
        an abb_rev_iation has the same result, as (here) does the ESC key. If "Implicit ..." is checked then sequ_ences of <i>hex digits</i> immediately
        followed by <b>uu</b> are
        imp_licitly taken as abb_rev_iations for the corr_esponding unicode character.
      </p>
      <p hang="Right-to-Left: ">
        The field handles  keys mapped to right-to-left characters (eg non-vowelled Hebrew or Arabic).
        The underlying rep_resent_ation of such a char_acter with codepoint  <i>c</i> is as a "mixed direction"
        composite: <tt>RLM<i> c </i>LRM</tt>.</p>
      <p align="center">
        (See <tt>https://www.w3.org/TR/WCAG20-TECHS/H34.html</tt>)
      </p>
      <p hang="Emojis:        ">
        The field handles emojis -- including ZWJ emojis -- whether defined as abbreviated substitutions or introduced by paste_ing as a <i>single
        element</i>.
      </p>
      <fill/>
      </body>
  )
}


