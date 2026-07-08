package org.sufrin.utility


/**
 * Represents an invariant abstract sequence of T `past++future` at a current position, where
 * `past` is not accessible. The `current` element is (defined to be) the first (if any) of
 * the `future` elements.
 * {{{
 *   PROTOCOL
 *     ((hasCurrent; current*); next)*
 * }}}
 * @tparam T
 */
trait Cursor[T] {
  /** Is the current element well-defined?
   * {{{ RETURNS future.nonEmpty
   * }}}
   */
  def hasCurrent:    Boolean
  /** The current element.
   *  {{{ PRE hasCurrent
   *      RETURNS future.head
   *  }}}
   */
  def current:       T

  /** Set the current element  by replacing the first (if any) future element.
   *  {{{
   *    past   = OLD past
   *    future = List(element) ++ OLD future.drop(1)
   *  }}}
   */
  def current_=(element: T): Unit

  /** Move to the next element.
   * {{{ PRE  hasCurrent
   *     POST
   *        future = OLD future.tail
   *        past = OLD past ++ List(OLD future.head)
   * }}}
   *
   */
  def next(): Unit

  /**
   * Returns the longest initial prefix of `future` that satisfies `p`, and leaves
   * the `current` (if any) satisfying `!p`.
   * {{{
   *   RETURN prefix WHERE
   *       past++prefix++future == OLD(past++future)
   *       prefix.forall(p)
   *       future.take(1).forall(!p)
   * }}}
   * @param p
   *
   */
  def takeWhile(p: T => Boolean): Seq[T] = {
    val result = collection.mutable.ArrayBuffer[T]()
    while (hasCurrent && p(current)) {
      result append current
      next()
    }
    result.toSeq
  }

  def takeIf(p: T => Boolean): Seq[T] = {
    val result = collection.mutable.ArrayBuffer[T]()
    if (hasCurrent && p(current)) {
      result append current
      next()
    }
    result.toSeq
  }

  /**
   *
   * {{{
   *   PRE there is a prefix of future with properly nesting bra/ket ending in ket
   *   RETURNS prefix WHERE
   *      past++prefix++future = OLD(past++future)
   *      prefix has properly nesting bra/ket ending in ket
   * }}}
   *
   * **Note** if the precondition is not met, the returned prefix is the whole `future`
   * @param bra
   * @param ket
   * @return
   */
  def takeNested(bra: T, ket: T): Seq[T] = {
    val result = collection.mutable.ArrayBuffer[T]()
    var count = 0
    var go = true
    while (go && hasCurrent) {
      if (current==bra) count += 1 else if(current==ket) { count -= 1; go = count>= 0 }
      if (go) {
        result append current
        next()
      }
    }
    result.toSeq
  }

  def takeNested2(bra: T, ket: T): Seq[T] = {
    val result = collection.mutable.ArrayBuffer[T]()
    var count = 0
    var go = true
    var last: T = bra
    var doubled = hasCurrent && current==bra
    if (doubled) next()
    while (go && hasCurrent) {
      if (current==bra) count += 1
      else
      if (current==ket) {
        count -= 1
        go = if (doubled) {
          last != ket
        } else count >= 0
      }
      if (go) {
        last = current
        result append current
        next()
      }
    }
    result.toSeq
  }

  /**
   * {{{
   *   == takeWhile(p).map(f)
   * }}}
   * @param p
   * @param f
   * @tparam U
   * @return
   */
  def mapWhile[U](p: T => Boolean)(f: T=>U): Seq[U] = {
    val result = collection.mutable.ArrayBuffer[U]()
    while (hasCurrent && p(current)) {
      result append f(current)
      next()
    }
    result.toSeq
  }

  /**
   * {{{
   *   == takeWhile(f(_).isDefined).map(f)
   * }}}
   * @param p
   * @param f
   * @tparam U
   * @return
   */
  def mapWhile[U](f: PartialFunction[T,U]): Seq[U] = {
    val result = collection.mutable.ArrayBuffer[U]()
    var go = true
    while (go && hasCurrent) {
      try {
        result append f(current)
        next()
      } catch {
        case _: MatchError => go = false
      }
    }
    result.toSeq
  }

  def dropWhile(p: T => Boolean): Unit = {
    while (hasCurrent && p(current)) {
      next()
    }
  }
}

/**
 * A `Cursor` face for iterator
 * @param iterator
 * @tparam T
 */
class CursorOfIterator[T](iterator: Iterator[T]) extends Cursor[T] {
  def hasCurrent: Boolean =
    buffer.isDefined || (iterator.hasNext && { buffer=Some(iterator.next()); true })
  private var buffer: Option[T] = None
  def current: T = buffer.get
  def current_=(element: T): Unit = buffer = Some(element)
  def next(): Unit = {
    buffer = None
    hasCurrent
    ()
  }
}

/**
 * A `Cursor` face for iterator
 * @param iterator
 * @tparam T
 */
final class CursorOfIterable[T](iterable: Iterable[T]) extends CursorOfIterator[T](iterable.iterator)

object Cursor {
  def apply[T](iterator: Iterator[T]): Cursor[T] = new CursorOfIterator(iterator)
  def apply[T](iterable: Iterable[T]): Cursor[T] = new CursorOfIterable(iterable)
}
