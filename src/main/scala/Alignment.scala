package org.sufrin.glyph

/**
 *  An `Alignment` describes alignments used when justifying paragraphs.
 *  Its `left, right` scalars describe the stretchability of the spaces
 *  that will be placed at the left and right of each line of a justified
 *  paragraph. Its `last` scalar is the stretchability at the right of
 *  the final line of the paragraph (which may have fewer words in it
 *  than the others.
 *
 * @see text
 *
 */
trait Alignment {
  import GlyphTypes.Scale
  type Fills = (Scale, Scale, Scale)
  val left: Scale
  val right: Scale
  val last: Scale

  /** A fresh stretchable glyph of zero length and `left` stretchability */
  def leftFill() = FixedSize.Space(0, left)

  /** A fresh stretchable glyph of zero length and `right` stretchability */
  def rightFill() = FixedSize.Space(0, right)

  /** A fresh stretchable glyph of zero length and `last` stretchability */
  def lastFill() = FixedSize.Space(0, last)
}

case object Left extends Alignment {
  val (left, right, last): Fills = (0f, 30000f, 30000f)
}

case object Right extends Alignment {
  val (left, right, last): Fills = (30000f, 0f, 0f)
}

case object Center extends Alignment {
  val (left, right, last): Fills = (30000f, 30000f, 30000f)
}

case object Justify extends Alignment {
  val (left, right, last): Fills = (0f, 0f, 30000f)
}

trait VAlignment
case object Top extends VAlignment
case object Mid extends VAlignment
case object Bottom extends VAlignment

