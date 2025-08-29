package org.sufrin
package glyph
package styled

import org.sufrin.glyph.CodePointSeqMap.CodePointSeq
import org.sufrin.utility.TextAbbreviations
import org.sufrin.SourceLocation.SourceLocation


/**
 *  A fixed-width reactive glyph that can be edited from the keyboard. The width of
 *  the glyph is `size * em`, where `em` is the size of an "m" in the specified font.
 *  The textlayout being edited can be of any length.
 *  It is panned, if necessary, to keep the cursor in view.
 *  Simple visual indications are given at each end of the glyph
 *  when there is non-visible textlayout at that end.
 *
 *  When the mouse cursor enters this glyph, it grabs the keyboard focus, and this
 *  directs subsequent keystrokes to it.
 *
 *  When the mouse cursor leaves this glyph, it gives up the keyboard focus.
 *
 */

/**
 * Unstyled TextField companion object.
 *
 * @see styled.TextField
 */
/** Styled and unstyled TextFields are implemented by the same class. */
object TextField {


  import GlyphTypes.Font
  import io.github.humbleui.jwm.EventKey

  val reportNewGlyph: (String, CodePointSeq) => Unit = glyph.TextField.reportNewGlyph

  def apply(onEnter: String => Unit            = { case text: String => },
            onError: (EventKey, Glyph) => Unit = glyph.TextField.popupError(_,_),
            onCursorLeave: String=>Unit        = { case text: String => },
            onChange: Option[String => Unit]   = None,
            size:    Int,
            initialText: String = "",
            abbreviations: TextAbbreviations = null,
            glyphcountData: PolyCodings = PolyCodings(),
            onNewGlyph: (String, CodePointSeq) => Unit = { (_, _) => }

  )
           (implicit sheet: StyleSheet): TextField = {
    val fg: Brush = sheet.textForegroundBrush
    val bg: Brush = sheet.textBackgroundBrush
    val font: Font = sheet.textFont
    new TextField(fg, bg, font, onEnter, onError, onCursorLeave, onChange, size, initialText, abbreviations, glyphcountData, onNewGlyph)
  }

  def helpText(style: StyleSheet): Glyph = {
    import glyphML._
    val language: language = Translator().withPackage(TransformsPackage)(style)
    import language._
    def SOURCE(implicit source: SourceLocation): String = source.toString()
    implicit val thisStyle: StyleSheet = style

    <body width="70em" align="justify" fg="blue" source={SOURCE} fontScale="0.7">
      <macro tag="itemize"><row>&nbsp;<col><?body?></col></row></macro>
      <macro tag="item" ><p hang="•" parIndent="1em"><?body?></p></macro>
      <p>
        Interaction with a simple text field is (more or less) standard, except for setting the text selection.
      </p>
      <itemize>
        <item>The cursor is shown as an I-beam and always kept in view</item>
        <item>The selection has a coloured background</item>
        <item>Visual cues are given when there is out-of-sight text</item>
        <item>The usual cut, copy, paste, and navigate keys are provided:</item>
      <itemize>
        <item>Home, End, Left, Right, and Backspace - usual effect</item>
        <item>Ctrl/Cmd Backspace - swaps the two characters before the cursor </item>
        <item>Ctrl/Cmd C - copy the selection (default all) to clipboard</item>
        <item>Ctrl/Cmd X - cut the selection (default all) to clipboard</item>
        <item>Ctrl/Cmd V - insert from clipboard</item>
        <item>Mousebutton - set the cursor here</item>
        <item>The text selection is between the <b>mark</b> (if any) and the cursor)</item>
        <p align="center"><b>Setting the mark</b></p>
        <itemize>
          <item>Secondary mousebutton - set the mark here</item>
          <item>Ctrl/Cmd mousebutton - set the mark here</item>
          <item>Ctrl/Cmd down - set the mark at the cursor</item>
          <item>Ctrl/Cmd+Shift down - unset the mark</item>
          <item>Ctrl/Cmd S - swap the mark and cursor</item>
        </itemize>
      </itemize>
      </itemize>
    </body>
  }

}