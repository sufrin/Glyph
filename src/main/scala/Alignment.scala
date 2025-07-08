package org.sufrin.glyph

/**
 *  A (horizontal)  `Alignment` is a descriptor used when justifying paragraphs.
 *  Its `left, right` scalars describe the stretchability of the spaces
 *  that will be placed at the left and right of each line of a justified
 *  paragraph. Its `last` scalar is the stretchability at the right of
 *  the final line of the paragraph (which may have fewer words in it
 *  than the others.
 *
 *  Alignments also describe how column elements are set within columns. The
 *  alignment calculation for `glyph` when locating it in a column of width `w`
 *  with a given `alignment` is
 *
 *  {{{glyph.location.x = alignment.proportion*(w-glyph.w)}}}
 *
 * @see text
 * @see Col
 *
 */
trait Alignment {
  import GlyphTypes.Scale
  type Fills = (Scale, Scale, Scale)
  val left: Scale
  val right: Scale
  val last: Scale

  /**  Proportion of excess space to use when Col setting */
  val proportion: Float

  /** A fresh stretchable glyph of zero length and `left` stretchability */
  def leftFill() = FixedSize.Fill(0, left)

  /** A fresh stretchable glyph of zero length and `right` stretchability */
  def rightFill() = FixedSize.Fill(0, right)

  /** A fresh stretchable glyph of zero length and `last` stretchability */
  def lastFill() = FixedSize.Fill(0, last)
}

case object Left extends Alignment {
  val (left, right, last): Fills = (0f, 30000f, 30000f)
  override val proportion: Float = 0f
}

case object Right extends Alignment {
  val (left, right, last): Fills = (30000f, 0f, 0f)
  override val proportion: Float = 1f
}

case object Center extends Alignment {
  val (left, right, last): Fills = (30000f, 30000f, 30000f)
  override val proportion: Float = 0.5f

}

case object Justify extends Alignment {
  val (left, right, last): Fills = (0f, 0f, 30000f)
  override val proportion: Float = 1f // meaningless when Column setting
}


/**
 * A (vertical) `VAlignment` is the vertical analogue of a horizontal `Alignment`. Used when
 * laying out `Row`s.
 */
trait VAlignment {
  /**  Proportion of excess space to use when Row setting */
  val proportion: Float
  /** Are baselines to be aligned (independent of proportion) */
  val atBaseline: Boolean = false
}
case object Top extends VAlignment {
  override val proportion: Float = 0f
}
case object Mid extends VAlignment {
  override val proportion: Float = 0.5f
}
case object Bottom extends VAlignment {
  override val proportion: Float = 1f
}
case object Baseline extends VAlignment {
  override val proportion: Float = 1f
  override val atBaseline: Boolean = true
}


