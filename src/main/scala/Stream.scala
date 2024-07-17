package org.sufrin.glyph


/**
 * `Iterator`-like structure supporting inspection of  the "current" `element` of
 * a sequential structure, providing `hasElement()` is true. Unlike an `Iterator`,
 * the current element can be inspected without being "consumed".
 */

trait Stream[T] {
  /** Is the current `element` well-defined */
  def hasElement:    Boolean
  def element:       T
  /** Move to the next element */
  def nextElement(): Unit
}

/**
 * A streaming face for `seq`
 * @param seq
 * @tparam T
 */
final class StreamSeq[T](seq: Seq[T]) extends Stream[T] {
  private var pos: Int = 0
  /** Is `element` defined */
  def hasElement: Boolean = 0 <= pos && pos < seq.length
  def element: T = seq(pos)
  def nextElement(): Unit = pos += 1
  def prevElement(): Unit = pos -= 1
}

final class StreamList[T](list: List[T]) extends Stream[T] {
  private var seq = list
  /** Is `element` defined */
  def hasElement: Boolean = seq.nonEmpty
  def element: T = seq.head
  def nextElement(): Unit = seq = seq.tail
}

/**
 * A streaming face for iterator
 * @param iterator
 * @tparam T
 */
class StreamIterator[T](iterator: Iterator[T]) extends Stream[T] {
  def hasElement: Boolean =
    buffer.isDefined ||
    (iterator.hasNext && { buffer=Some(iterator.next()); true })
  var buffer: Option[T] = None
  def element: T = buffer.get
  def nextElement(): Unit = {
    buffer = None
    hasElement
    ()
  }
}

final class StreamIterable[T](iterable: Iterable[T]) extends StreamIterator[T](iterable.iterator)

object Stream {
  def apply[T](list: List[T]): Stream[T] = new StreamList(list)
  def apply[T](seq: Seq[T]): Stream[T]   = new StreamSeq(seq)
  def apply[T](iterator: Iterator[T]): Stream[T] = new StreamIterator(iterator)
  def apply[T](iterable: Iterable[T]): Stream[T] = new StreamIterable(iterable)
}