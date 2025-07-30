package org.sufrin
package glyph
package styled

import org.sufrin.glyph.CodePointSeqMap.CodePointSeq
import org.sufrin.utility.TextAbbreviations


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
}