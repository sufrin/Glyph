package org.sufrin.glyph


/**
 * `Iterator`-like structure supporting inspection ofPaint  the "current" `element` ofPaint
 * a sequential structure, providing `hasElement` is true. Unlike an `Iterator`,
 * the current element can be inspected without being "consumed".
 *
 * Nothing is guaranteed about `element` unless `hasElement` yields true.
 * The method `setElement(t)` replaces the stream's view ofPaint  its current element with `t`,
 * and makes `hasElement` yield true.
 */

trait Stream[T] {
  /** Is the current `element` well-defined */
  def hasElement:    Boolean
  def element:       T
  /** Set the element */
  def setElement(t: T): Unit
  /** Move to the next element */
  def nextElement(): Unit
}

/**
 * A streaming face for iterator
 * @param iterator
 * @tparam T
 */
class StreamIterator[T](iterator: Iterator[T]) extends Stream[T] {
  def hasElement: Boolean =
    buffer.isDefined || (iterator.hasNext && { buffer=Some(iterator.next()); true })
  private var buffer: Option[T] = None
  def element: T = buffer.get
  def setElement(element: T): Unit = buffer=Some(element)
  def nextElement(): Unit = {
    buffer = None
    hasElement
    ()
  }
}

final class StreamIterable[T](iterable: Iterable[T]) extends StreamIterator[T](iterable.iterator)

object Stream {
  def apply[T](iterator: Iterator[T]): Stream[T] = new StreamIterator(iterator)
  def apply[T](iterable: Iterable[T]): Stream[T] = new StreamIterable(iterable)
}