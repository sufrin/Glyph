package org.sufrin.glyph
package tests

import styled.BookSheet
import unstyled.static
import Brushes.{black, blackFrame, lightGrey, white}

import io.github.humbleui.jwm.Key
import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.styles.decoration.RoundFramed
import org.sufrin.logging

object TextField extends Application {
  val title: String = "TextField test"

  implicit val style: StyleSheet = StyleSheet(textFontSize=25, textFontFamily=FontFamily("Menlo"), textBackgroundBrush = Brushes.white)
  val anchor = static.INVISIBLE()

  val controlsStyle: StyleSheet =
      style.copy(fontScale=0.85f, buttonDecoration = RoundFramed(fg=Brushes.darkGrey(width=4), bg=Brushes.lightGrey, enlarge=0.3f, radius=10))

  val abbreviations = new TextAbbreviations(onLineTrigger = true, implicitUnicode = false, onAmbiguous = TextAbbreviations.ambiguous)
  locally {
    abbreviations.reversible = true
    for {  (abbr, symb) <- StockAbbreviations.all } abbreviations.update(abbr, symb)(SOURCE)
  }

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
                     onEnter = showDimensions,
                     onCursorLeave = { _ => anchor.guiRoot.giveupFocus() },
                     onError = {
                       case (eventKey, _) =>
                         import Modifiers._
                         logging.SourceDefault.warn(s"Undefined keystroke: ${eventKey.asShortString}")},
                     abbreviations = abbreviations,
                     onNewGlyph = styled.TextField.reportNewGlyph(_,_),
                     initialText = "")(style.copy(fontScale = 1.1f)).withAbbreviationKey(Key.ESCAPE)

  defs("TEXTFIELD") = _=>textField.framed()

  val helpText: Glyph =
    <body width="70em" align="justify" fg="blue" parSkip="0.75em" itemwidth="60em" itemindent="2em" itemalign="justify" source={SOURCE}>
      No help, I'm afraid.
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
      windowdialogues.Dialogue.FLASH(content.enlarged(20)).OnRootOf(anchor, Vec(50,50)).start()
  }(controlsStyle)

  defs("UNICODE") = _ => styled.TextButton("Unicode", hint=Hint(5, "Replace glyph at cursor left\nby unicode codepoint(s)")) {
    _ => textField.unicode()
  }(controlsStyle)

  defs("UNABBR") = _ => styled.TextButton("Abbr", hint=Hint(5, "Replace cursor left material\nby its abbreviation\n(inverse of 'abbreviate')")) {
    _ => textField.unabbreviation()
  }(controlsStyle)

  def showDimensions(text: String): Unit = {
    val t=unstyled.Text(text, style.textFont)
    println(s"$text(${t.w},${t.h}) height=${t.height} a=${t.ascent}, desc=${t.descent} l=${t.leading} s=${t.spacing} drop=${t.drop}")
  }


  val GUI: Glyph = NaturalSize.Col(align=Center, bg=Brushes.white)(
    anchor,
    //textField,
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
  ).enlarged(20)
}


