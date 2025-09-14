package org.sufrin.glyph
package cached

import scala.reflect.ClassTag

/**
 * Effectively a cached view of `seq.andThen(element)`.
 */
class CachedSeq[T,U: ClassTag](seq: Seq[T], element: T => U) extends Seq[U] { host =>
  final val UNDEF: U = null.asInstanceOf[U]

  /** Invariant length of the derived sequence */
  def length: Int = seq.length

  private final val cache = Array.ofDim[U](length)

  /**
   * The value of the derived sequence at `i` providing `i<length`.
   * Pragmatically: if `element(seq(i))` yields null, then
   * that value will be returned "correctly", but it will
   * be recalculated at the next invocation of `apply` at
   * that `i`.
   */
  def apply(i: Int): U = {
    var r = cache(i)
    if (r==UNDEF)  {
      r = element(seq(i))
      cache(i)=r
    }
    r
  }

  /**
   *  Invalidate the entire cache at all its indexes.
   * @see clear
   */
  def clear(): Unit = for {i<-0 until length} cache(i)=UNDEF

  /**
   *  Invalidate the cache at index `i`.
   *  The next access to the derived sequence at `i` will force
   *  a recalculation of its value.
   */
  def clear(i: Int): Unit = cache(i)=UNDEF

  /**
   * @return an iterator on the current cached value.
   */
  def iterator: Iterator[U] = new Iterator[U] {
    var i=0
    def hasNext: Boolean = i<host.length
    def next(): U = {
      val r = host.apply(i)
      i += 1
      r
    }
  }
}

object CachedSeq {
  def apply[T,U: ClassTag](seq: Seq[T])(element: T=>U): CachedSeq[T,U] = new CachedSeq[T,U](seq, element)
}
