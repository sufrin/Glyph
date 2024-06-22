package org.sufrin.glyph
package tests

import GlyphTypes.FontStyle.NORMAL
import GlyphTypes.{FontManager, Typeface}
import Styles.Decoration
import DefaultBrushes._


object PortmanteauStyle extends Styles.Sheet {
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Arial", NORMAL)
  implicit val ButtonStyle: Styles.ButtonStyle =
    buttonStyle.copy(frame=Decoration.Blurred(blue, nothing, 15f, 5f),
      up=buttonStyle.up.copy(fg=white))
}
