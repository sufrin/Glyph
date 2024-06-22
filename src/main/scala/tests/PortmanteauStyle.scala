package org.sufrin.glyph
package tests
import GlyphTypes._
import FontStyle._
import Styles.Decoration
import DefaultBrushes._



object PortmanteauStyle extends Styles.Sheet {
  override lazy val face: Typeface = FontManager.default.matchFamilyStyle("Courier", NORMAL)
  override lazy val buttonFontSize: Scalar = 28
  override lazy val buttonStyle: Styles.ButtonStyle =
     Styles.DefaultSheet.buttonStyle.copy(frame=Decoration.Blurred(blue, nothing, 15f, 5f),
                                          up=Styles.DefaultSheet.buttonStyle.up.copy(fg=white))
}

