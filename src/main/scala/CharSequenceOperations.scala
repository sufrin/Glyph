
package org.sufrin
package utility


object CharSequenceOperations {

  def leftJustify(theSeq: CharSequence, width: Int, ch: Char=' '): CharSequence = if (theSeq.length==width) theSeq else new CharSequence{ thisSeq =>
    val length: Int = width
    def charAt(i: Int): Char = if (i<theSeq.length) theSeq.charAt(i) else ch
    def subSequence(start: Int, end: Int): CharSequence = (new Cat(theSeq, constantSeq(ch, width-theSeq.length))).subSequence(start, end) // lazy programmer
    lazy val toSeq: Seq[Char] = (0 until width).map(charAt)
    override lazy val toString: String = new String(toSeq.toArray)
  }

  def rightJustify(theSeq: CharSequence, width: Int, ch: Char=' '): CharSequence =  if (theSeq.length==width) theSeq else new CharSequence{
    val length: Int = width
    val delta = width-theSeq.length
    def charAt(i: Int): Char = if (i<delta) ch else theSeq.charAt(i-delta)

    def subSequence(start: Int, end: Int): CharSequence = (new Cat(constantSeq(ch, width-theSeq.length), theSeq)).subSequence(start, end) // lazy programmer

    lazy val toSeq: Seq[Char] = (0 until width).map(charAt)
    override lazy val toString: String = new String(toSeq.toArray)
  }

  def leftJustify(width: Int)(theSeq: CharSequence): CharSequence = leftJustify(theSeq, width)
  def rightJustify(width: Int)(theSeq: CharSequence): CharSequence = rightJustify(theSeq, width)
  def centerJustify(width: Int)(theSeq: CharSequence): CharSequence = if (theSeq.length==width) theSeq else {
    val extra = width-theSeq.length
    Cat(constantSeq(' ', extra/2), theSeq, constantSeq(' ', extra-extra/2))
  }


  def constantSeq(ch: Char, width: Int): CharSequence = new CharSequence {
    def length(): Int = width
    def charAt(index: Int): Char = ch
    def subSequence(start: Int, end: Int): CharSequence =
      if (start==end) this else constantSeq(ch, 0 max (end-start))
    lazy val  toSeq: Seq[Char] = (0 until width).map(charAt)
    override lazy val toString: String = new String(toSeq.toArray)
  }

  /** View a pair of `CharSequence`s as their catenation: constant space and time */
  class Cat(l: CharSequence, r: CharSequence) extends CharSequence {

    val length: Int = l.length+r.length

    def charAt(index: Int): Char = {
     if (index<l.length) l.charAt(index) else r.charAt(index-l.length)
    }

    /** "Efficient" view of the specified subsequence */
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
   *  Distributed catenation of `css`  `log2(css.length)` space
   *  and access time.
   */
  def Cat(c1: CharSequence, css: CharSequence*): CharSequence = Cat(css.prepended(c1))

  /**
   * Materialize `chars` as a `String`.
   */
  def asString(chars: CharSequence): String = {
    import WithCharSequenceOps._
    chars.asString
  }

  /** Equal sequences */
  def ====(l: CharSequence, r: CharSequence): Boolean = (l eq r) || {
        var eq = l.length == r.length
        var i = 0
        while (i<l.length&&eq) {
          eq=l.charAt(i)==r.charAt(i)
          i += 1
        }
        eq
    }

  /**
   * Hashcode computed from `cs.drop(cs.length-15)` using the same algorithm (as it happens)
   * as `String.hashCode`.
   */
  def ####(cs: CharSequence): Int = {
    cs.length match {
      case 0 => 0
      case 1 => 31*cs.charAt(0).toInt
      case n =>
        var h: Int = 0
        var i: Int = (n-15) max 0
        while (i!=n) {
          h = 31*h + cs.charAt(i).toInt
          i+=1
        }
        h
    }
  }


  implicit class WithCharSequenceOps(val chars: CharSequence) extends AnyVal {

    def leftJustify(width: Int): CharSequence =
      if (false)
        new Cat(chars, constantSeq(' ', width-chars.length))
      else
        CharSequenceOperations.leftJustify(chars, width, ' ')

    def centerJustify(width: Int): CharSequence = Cat(constantSeq(' ', (width-chars.length)/2), chars, constantSeq(' ', (width-chars.length)/2))

    def rightJustify(width: Int): CharSequence = {
        if (false)
          new Cat(constantSeq(' ', width-chars.length),  chars)
        else
          CharSequenceOperations.rightJustify(chars, width, ' ')
    }

    def map[T](f: Char=>T): Seq[T] = (0 until chars.length).map{i=>f(chars.charAt(i))}

    def ++(otherChars: CharSequence): CharSequence = new Cat(chars, otherChars)

    def cat(others: CharSequence*): CharSequence = Cat(chars +: others )

    @inline def foreach (op: Char=>Unit): Unit = for { c<-iterator } op(c)

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

    @inline def toSeq: Seq[Char] = toIndexedSeq

    def asString: String = new String(chars.toSeq.toArray)

    /** An iterator that yields '\u0000' when `next()` is used after `hasNext` yields false */
    def forwardIterator: Iterator[Char] = new Iterator[Char] {
      var i: Int = 0
      @inline def hasNext: Boolean = i < chars.length
      @inline def next(): Char =
        if (hasNext) { val c = chars.charAt(i); i += 1; c } else '\u0000'
    }

    /** A reverse iterator that yields '\u0000' when `next()` is used after `hasNext` yields false */
    def reversedIterator: Iterator[Char] = new Iterator[Char] {
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


    @inline private def `16x`(n: Long): Long = n<<4
    @inline private def `16x`(n: Int): Int = n<<4

    /**
     *  Map a well-formed hexit sequence to Some(`Long`)
     *  else yield `None`
     */
    def hexToLong: Option[Long] = {
      var n: Long    = 0
      var wellFormed = true
      for { c <- chars }
        if ('a'<=c&&c<='f')  n = `16x`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16x`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16x`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n) else None
    }

    /**
     *  Map a well-formed hexit sequence to Some(`Long`)
     *  else yield `None`
     */
    def hexToInt: Option[Int] = {
      var n: Int    = 0
      var wellFormed = true
      for { c <- chars }
        if ('a'<=c&&c<='f')  n = `16x`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16x`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16x`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n) else None
    }

    /**
     *  Map a well-formed unicoded escape sequence `\uxxxx` sequence to `Some(Char)`
     *  else yield `None`
     */
    def toUnicode: Option[Char] = {
      var n: Long = 0
      val it         = chars.forwardIterator
      val slosh      = it.next()
      val u          = it.next()
      var wellFormed = slosh=='\\' && (u=='u' || u=='U')
      if (wellFormed) for { c <- it }
        if ('a'<=c&&c<='f')  n = `16x`(n)+c-'a'+10 else
          if ('A'<=c&&c<='F')  n = `16x`(n)+c-'A'+10 else
            if ('0'<=c&&c<='9')  n = `16x`(n)+c-'0' else wellFormed = false
      if (wellFormed) Some(n.toChar) else None
    }
  }

}

object TestCharSequenceOps {
  import CharSequenceOperations._
  def main(args: Array[String]): Unit = {
    val l0 = constantSeq('=', 5)
    println(l0.toSeq)
    val l1  = "01234".leftJustify(10)
    val l1a = Cat(List("01234",l0))
    val l2 = leftJustify("01234", 10, '=')
    println(l1.length)
    println(l1a.length)
    println(l2)

    val m1="56789".rightJustify(10)
    println(m1.length)
    val m2 = CharSequenceOperations.rightJustify("56789", 10, 'x')
    println(m1, m2)

    println(m1.subSequence(4, 7).asString)
    println(m2.subSequence(4, 7).asString)
    println(l1.subSequence(4, 7).asString)
    println(l2.subSequence(4, 7).asString)
  }
}
