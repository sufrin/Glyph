package org.sufrin.glyph
package tests
import DefaultBrushes._
import GlyphTypes._
import GlyphTypes.FontStyle._



object PortmanteauStyle extends Styles.BlurredSheet {

  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Courier", NORMAL)
  override lazy val buttonFontSize: Scalar = 28

}

