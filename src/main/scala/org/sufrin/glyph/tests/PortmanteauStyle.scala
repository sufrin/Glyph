package org.sufrin.glyph.tests

import org.sufrin.glyph.GlyphTypes.FontStyle.NORMAL
import org.sufrin.glyph.GlyphTypes.{FontManager, Typeface}
import org.sufrin.glyph.Styles
import org.sufrin.glyph.Styles.Decoration
import org.sufrin.glyph.DefaultBrushes._


object PortmanteauStyle extends Styles.Basic {
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Arial", NORMAL)
  implicit val ButtonStyle: Styles.ButtonStyle =
    buttonStyle.copy(frame=Decoration.Blurred(blue, nothing, 15f, 5f),
      up=buttonStyle.up.copy(fg=white))
}
