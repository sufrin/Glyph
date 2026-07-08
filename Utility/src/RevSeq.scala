package org.sufrin.utility

/**
 * A lightweight implementation of a sequence that grows at the end (in unit time), and will
 * disgorge its elements (once) in the usual order in time linear in its length.
 * Designed for use  as a buffer when accumulating elements of a list as it is parsed.
 * {{{
 *  exprs: (RevSeq[Expr]) =
 *             expr            { RevSeq($expr) }
 *         |   exprs `,` expr  { $exprs :: $expr }
 *
 *  bracketed: Seq[Expr] =
 *          '(' exprs ')' { $exprs.toSeq }
 * }}}
 *
 * @param elts initial elements of the sequence
 * @tparam T
 */
class RevSeq[T](elts: Seq[T]) {
  /** Represents
   *  {{{
   *    seq: Seq[T]
   *  }}}
   *
   *
   * Abstraction
   * {{{
   *    abs: Seq[T] = reversed.reverse
   * }}}
   */

  // initially: seq = elts
  var reversed: List[T] = elts.toList

  /**
   * Scala notation: both `this:+ elt`, or `this.:+(elt)`
   * ensure:
   * {{{
   *   seq = `seq ++ [elt]
   *          returns this
   * }}}
   */
  def :+ (elt: T): this.type = {
    reversed = elt :: reversed
    this
  }

  /**
   * In scala notation: `elt :: this` behaves as  `this :+ elt`
   */
  def ::(elt: T): this.type = {
    reversed = elt :: reversed
    this
  }

  def toList: List[T] = {
    val result = reversed.reverse
    reversed = Nil // mitigate space leaks
    result
  }

  def toSeq: Seq[T] = {
    val result = reversed.reverse.toSeq
    reversed = Nil // mitigate space leaks
    result
  }

}

object RevSeq {
  def apply[T]()      = new RevSeq[T](Nil)
  def apply[T](elt:T) = new RevSeq[T](List(elt))
}
