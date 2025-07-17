
package org.sufrin
package utility

import scala.:+
import scala.annotation.unused


object CharSequenceOperations {

  /** View a pair of `CharSequence`s as their catenation: constant space and time */
  class Cat(l: CharSequence, r: CharSequence) extends CharSequence {

    val length: Int = l.length+r.length

    def charAt(index: Int): Char = {
     if (index<l.length) l.charAt(index) else r.charAt(index-l.length)
    }

    /** Constant space and time view of the specified subsequence */
    def subSequence(start: Int, end: Int): CharSequence = {
      if (start==0 && end==length) this else
      if (start<=l.length && end<=l.length) l.subSequence(start, end) else {
        val rs=start-l.length
        val re=end-l.length
        if (0<=rs && re<=r.length) r.subSequence(rs, re) else
        new Cat(l.subSequence(start, l.length), r.subSequence(0, re))
      }
    }
  }

  /**
   * The empty `CharSequence`
   */
  object empty extends CharSequence {

    def length(): Int = 0

    def charAt(index: Int): Char = throw new IllegalArgumentException(s"empty: CharSequence.charAt($index)")

    def subSequence(start: Int, end: Int): CharSequence = {
      assert(start==0 && end==0, s"empty: CharSequence.subSequence($start, $end)")
      this
    }
  }

  /**
   *  Distributed catenation of `css`  `log2(css.length)` space
   *  and access time.
   */
  def Cat(css: Seq[CharSequence]): CharSequence = {
    if (css.length==0) empty else
    if (css.length==1) css(0) else
    if (css.length==2) new Cat(l=css(0), r=css(1)) else {
      new Cat(l=Cat(css.take(css.length/2)), r=Cat(css.drop(css.length/2)))
    }
  }

  /**
   * Materialize `cs` as a `String`. Allocates an array of size `cs.length`
   * @param cs
   * @return
   */
  def asString(cs: CharSequence): String = {
    val array = new Array[Char](cs.length)
    val seq = cs.toIndexedSeq
    seq.copyToArray(array)
    new String(array)
  }

  implicit class WithCharSequenceOps(val chars: CharSequence) extends AnyVal {

    def map[T](f: Char=>T): Seq[T] = (0 until chars.length).map{i=>f(chars.charAt(i))}

    def ++(otherChars: CharSequence): CharSequence = new Cat(chars, otherChars)

    def cat(others: CharSequence*): CharSequence = Cat(chars +: others )

    def foreach (op: Char=>Unit): Unit = for { c<-forwardIterator() } op(c)

    /** A strict iterator that just fails if `next()` is used after `hasNext` yields false */
    def iterator: Iterator[Char] = new Iterator[Char] {
      var i: Int = 0
      @inline def hasNext: Boolean = i < chars.length
      @inline def next(): Char = {
        assert(hasNext)
        val c = chars.charAt(i); i += 1; c
      }
    }

    def toIndexedSeq: IndexedSeq[Char] = new IndexedSeq[Char] {
      def apply(i: Int): Char = chars.charAt(i)
      def length: Int = chars.length
    }

    def toSeq: Seq[Char] = toIndexedSeq

    /** An iterator that yields '\u0000' when `next()` is used after `hasNext` yields false */
    def forwardIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = 0
      @inline def hasNext: Boolean = i < chars.length
      @inline def next(): Char =
        if (hasNext) { val c = chars.charAt(i); i += 1; c } else '\u0000'
    }

    /** A reverse iterator that yields '\u0000' when `next()` is used after `hasNext` yields false */
    def reversedIterator(): Iterator[Char] = new Iterator[Char] {
      var i: Int = chars.length
      @inline def hasNext: Boolean = i > 0
      @inline def next(): Char = if (hasNext)  { i -= 1; chars.charAt(i) } else '\u0000'
    }

    /** An efficient reverse iterator over `chars.subSequence(0, upTo)` that yields '\u0000' when used after hasNext` yields false */
    def reversedIterator(upTo: Int): Iterator[Char] = new Iterator[Char] {
      var i: Int = upTo
      @inline def hasNext: Boolean = i > 0
      @inline def next(): Char = if (hasNext) { i -= 1; chars.charAt(i) } else '\u0000'
    }

    @inline private def `16`(n: Long): Long = n<<4
    @inline private def `16`(n: Int): Int = n<<4

    /**
     *  Map a well-formed hexit sequence to Some(`Long`)
     *  else yield `None`
     */
    def hexToLong: Option[Long] = {
      var n: Long    = 0
      var wellFormed = true
      for { c <- chars.forwardIterator() }
        if ('a'<=c&&c<='f')  n = `16`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n) else None
    }

    /**
     *  Map a well-formed hexit sequence to Some(`Long`)
     *  else yield `None`
     */
    def hexToInt: Option[Int] = {
      var n: Int    = 0
      var wellFormed = true
      for { c <- chars.forwardIterator() }
        if ('a'<=c&&c<='f')  n = `16`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n) else None
    }

    /**
     *  Map a well-formed unicoded escape sequence `\uxxxx` sequence to `Some(Char)`
     *  else yield `None`
     */
    def toUnicode: Option[Char] = {
      var n: Long = 0
      val it         = chars.forwardIterator()
      val slosh      = it.next()
      val u          = it.next()
      var wellFormed = slosh=='\\' && (u=='u' || u=='U')
      if (wellFormed) for { c <- it }
        if ('a'<=c&&c<='f')  n = `16`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n.toChar) else None
    }
  }

}
