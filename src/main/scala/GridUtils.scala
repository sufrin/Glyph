package org.sufrin.glyph

/**
 * Utilities for building grids from sequences
 */
object GridUtils {
  /** The rows of a sequence presented in row order  */
  def byRow[T](width: Int, seq: Seq[T]):  Seq[Seq[T]] =
    seq.sliding(width, width).toSeq

  /** The *rows, columns) of a sequence presented in row order, padded with padding so that all columns are the same height  */
  def byCol[T](padding: T)(width: Int, seq: Seq[T]):  (Seq[Seq[T]], Seq[Seq[T]]) = {
    val deficit = seq.length % width
    val rows =
    if (deficit==0)
      byRow(width, seq)
    else {
      val pad = for { _ <-0 until width-deficit } yield padding
      byRow(width, seq ++ pad)
    }
    (rows, rows.transpose)
  }

  implicit def productToSeq[T](p: Product): Seq[T] = p.productIterator.toSeq.asInstanceOf[Seq[T]]

  def rows[T](rs: Seq[Seq[T]]): Seq[Seq[T]] = rs

  def main(args: Array[String]): Unit = {
    val (rs, cs) = byCol("X")(5, "a b c d e f g h i j k l".split(' ').toList)
    println(rs)
    println(cs)

    {
      val (rs, cs) = byCol("X")(3, "a b c d e f g h i j k l".split(' ').toSeq)
      println(rs)
      println(cs)
    }
  }
}
