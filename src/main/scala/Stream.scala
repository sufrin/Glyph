package org.sufrin.glyph

/**
 * `Iterator`-like structure supporting inspection of  the "current" `element` of
 * a sequence, providing `hasElement()` is true. Unlike an `Iterator`,
 * the current element can be inspected without being "consumed".
 * The methods `nextElement` and `prevElement` change the current element position,
 * which starts at `0`.
 *
 * @param seq
 * @tparam T
 */
class Stream[T](seq: Seq[T]) {
  private var pos: Int = 0
  /** Is `element` defined */
  def hasElement: Boolean = 0 <= pos && pos < seq.length
  def element: T = seq(pos)
  def nextElement(): Unit = pos += 1
  def prevElement(): Unit = pos -= 1
}
