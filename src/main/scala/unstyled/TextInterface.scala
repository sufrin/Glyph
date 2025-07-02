package org.sufrin.glyph
package unstyled

import GlyphTypes.Scalar

import io.github.humbleui.types.Point

/**
 * Publicly useable features of a Text glyph.
 */
trait TextInterface extends Glyph {

  protected val implementation: io.github.humbleui.skija.TextLine

  /** The index of the character (Unicode codepoint) whose visual representation is laterally offset by `distance` from the
   * start of the displayed text.
   *
   * Gives wrong answer when surrogate-pairs are present
   */
  def charIndexOf(distance: Scalar): Int =
    implementation.getOffsetAtCoord(distance)

  /** The lateral offset from the start of the displayed text of the visual representation of the `index`th character
    * of the string.
    *
    * {{{
    *   charIndexOf(lateralOffsetOf(n)) = n (for 0<=n<string.length)
    * }}}
    */
  def lateralOffsetOf(index: Int): Scalar =
    implementation.getCoordAtOffset(index)

  /** Font-specific identifiers of the individual characters of the text. "These IDs
   * help in rendering text efficiently".
   *
   * Gives wrong answer when surrogate-pairs are present.
   */
  def glyphIDs: Seq[Short] = implementation.getGlyphs.toSeq
}
