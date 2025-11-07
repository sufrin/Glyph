package org.sufrin.glyph
package tests

import unstyled.static
import Brushes.{black, white}

import io.github.humbleui.jwm.Key
import org.sufrin.SourceLocation.SourceLocation
import org.sufrin.glyph.TextAbbreviations
import org.sufrin.logging

object TextField extends Application {
  val title: String = "TextField test"

  implicit val style: StyleSheet =
    StyleSheet(textFontSize    = 25,
               textFontFamily  = FontFamily("Menlo"),
               backgroundBrush = Brushes.transparent
               )

  lazy val anchor = static.INVISIBLE()

  val abbreviations = new TextAbbreviations(onLineTrigger = true, implicitUnicode = false, onAmbiguous = TextAbbreviations.ambiguous)
  locally {
    abbreviations.reversible = true
    for {  (abbr, symb) <- StockAbbreviations.all } abbreviations.update(abbr, symb)
  }

  import glyphML.Translator
  val language = Translator(style)
  import language._
  val defs = language.definitions

  val helpText: Glyph = styled.TextField.helpText(style)

  val frameGrey = Brushes.darkGrey(width=2)

  val hintSheet   = style.copy(fontScale=0.6f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val buttonSheet = style.copy(fontScale=0.8f, buttonDecoration = styles.decoration.RoundFramed(frameGrey, radius=20, enlarge=10))
  val controlsStyle: StyleSheet =
    buttonSheet//style.copy(fontScale=0.85f, buttonDecoration = RoundFramed(fg=Brushes.darkGrey(width=4), bg=Brushes.lightGrey, enlarge=0.3f, radius=10))

  val liveSubstitution: styled.ToggleVariable = styled.ToggleVariable(abbreviations.onLineTrigger)   { state => abbreviations.onLineTrigger = state }
  val implicitUnicode: styled.ToggleVariable  = styled.ToggleVariable(abbreviations.implicitUnicode) { state => abbreviations.implicitUnicode=state }

  def triggerButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable automatic substitution for abbreviations\nas they are typed.\n(SHIFT-SHIFT substitutes when disabled)")(hintSheet))(liveSubstitution)(buttonSheet)

  def implicitButton: Glyph =
    styled.CheckBox(initially=false,
                    hint=Hint(5, "Enable implicit abbreviations\nof unicode glyphs\nexpressed as hex digit sequences\n followed by \"uu\" ")(hintSheet))(implicitUnicode)(buttonSheet)


  defs("CONTROLS") =
    _=>
      NaturalSize.Row(align=Mid)(
        styled.Label("Live substitution: ")(controlsStyle), triggerButton,
        styled.Label("  Implicit unicode: ")(controlsStyle), implicitButton
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
    _ => styled.windowdialogues.Dialogue.FLASH(helpText.enlarged(20)).South(helpButton).start()
  }(controlsStyle)

  defs("HELPBUTTON") = _ => helpButton

  defs("SHOWBUTTON") = _ =>  styled.TextButton("Show", hint=Hint(5, "Show all the abbreviations")(hintSheet)){
    _ =>
      val order = new Ordering[String] { def compare(x: String, y: String): Int = x.compareToIgnoreCase(y) }
      val substitute =
        StockAbbreviations.all.toMap
      val pairs =
        StockAbbreviations.all.map(_._1).toSeq.sorted(order).
          flatMap{ key => List(styled.Label(key), styled.Label(substitute(key)))}
      val content =
        NaturalSize.Grid(fg=black, bg=white, padx=10, pady=5).table(width=28)(pairs)
      styled.windowdialogues.Dialogue.FLASH(content.enlarged(20)).OnRootOf(anchor, Vec(50,50)).start()
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

  def SOURCE(implicit source: SourceLocation): String = source.toString()

  lazy val GUI: Glyph = NaturalSize.Col(align=Center, bg=Brushes.white)(anchor above
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


