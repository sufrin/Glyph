package org.sufrin.glyph


/**
 * `Iterator`-like structure supporting inspection of  the "current" `element` of
 * a sequential structure, providing `hasElement` is true. Unlike an `Iterator`,
 * the current `element` can be inspected without being "consumed".
 *
 * Nothing is guaranteed about `element` unless `hasElement` yields true.
 * The method `pushBack(t)` replaces the stream's view of  its current element with `t`,
 * and makes `hasElement` yield true.
 */

trait PushbackIterator[T] {
  /** Is the current `element` well-defined */
  def hasElement:    Boolean
  def element:       T
  /** Set the element */
  def pushBack(t: T): Unit
  /** Move to the next element */
  def nextElement(): Unit
}

/**
 * A pushback face for iterator
 * @param iterator
 * @tparam T
 */
class PushbackIteratorOfIterator[T](iterator: Iterator[T]) extends PushbackIterator[T] {
  def hasElement: Boolean =
    buffer.isDefined || (iterator.hasNext && { buffer=Some(iterator.next()); true })
  private var buffer: Option[T] = None
  def element: T = buffer.get
  def pushBack(element: T): Unit = buffer=Some(element)
  def nextElement(): Unit = {
    buffer = None
    hasElement
    ()
  }
}

final class PushbackIteratorOfIterable[T](iterable: Iterable[T]) extends PushbackIteratorOfIterator[T](iterable.iterator)

object PushbackIterator {
  def apply[T](iterator: Iterator[T]): PushbackIterator[T] = new PushbackIteratorOfIterator(iterator)
  def apply[T](iterable: Iterable[T]): PushbackIterator[T] = new PushbackIteratorOfIterable(iterable)
}