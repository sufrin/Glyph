package org.sufrin.glyph

/**
 * Utilities for building grids from sequences
 */
object GridUtils {
  /** The rows of a sequence presented in row order  */
  def byRow[T](width: Int, seq: Seq[T]):  Seq[Seq[T]] =
    seq.sliding(width, width).toSeq

  /** The columns of a sequence presented in row order, padded with nulls so that all columns are the same height  */
  def byCol[T](height: Int, seq: Seq[T]):  Seq[Seq[T]] =
    byCol(null.asInstanceOf[T])(height, seq)

  /** The columns of a sequence presented in row order, padded with padding so that all columns are the same height  */
  def byCol[T](padding: T)(height: Int, seq: Seq[T]):  Seq[Seq[T]] = {
    val deficit = seq.length % height
    if (deficit==0)
      byRow(height, seq).transpose
    else {
      val pad = for { _ <-0 until height-deficit } yield padding
      byRow(height, seq ++ pad).transpose
    }
  }
}
