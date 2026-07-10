package org.sufrin.utility

/**
 * Sequential access to a collection of `T` using the protocol
 * {{{
 *  while (cursor.hasCurrent) { ...current...; cursor.next() }
 * }}}
 *
 * @tparam T
 */
trait Cursor[T] { hostCursor =>
  def hasCurrent: Boolean
  def current: T
  def next(): Unit

  /** Prefix of this iterator's  remaining collection whose members satisfy `p` */
  def takeWhile(p: T=>Boolean):Seq[T] = {
    var r: List[T] = Nil
    while (hasCurrent && p(current)) {
      r = current :: r
      next()
    }
    r.reverse
  }

  /** Drop the prefix of this iterator's  remaining collection whose members satisfy `p` */
  def dropWhile(p: T=>Boolean): Unit = {
    while (hasCurrent && p(current)) {
      next()
    }
  }

  /**  */
  def map[U](f: T=>U): Cursor[U] = new Cursor[U] {
    def hasCurrent: Boolean = hostCursor.hasCurrent
    var currentValue: Option[U] = None
    def current: U = currentValue match {
      case None =>
        val v=f(hostCursor.current)
        currentValue=Some(v)
        v
      case Some(v) => v
    }
    def next(): Unit = hostCursor.next()
  }

  /** The iterator corresponding to this cursor */
  def iterator: Iterator[T] =  new Iterator[T] {
    def hasNext: Boolean = hostCursor.hasCurrent
    def next(): T = {
      val v = hostCursor.current
      hostCursor.next()
      v
    }
  }

}

/** Cursor for the sequence `s` */
class SeqCursor[T](s: Seq[T]) extends Cursor[T] {
  var i=0
  def hasCurrent: Boolean = i<s.length
  def current: T = s(i)
  def next(): Unit = if (i<s.length) i+=1 else throw new IllegalStateException(s"SeqCursor@$i>=${s.length}")
  override def takeWhile(p: T=>Boolean):Seq[T] = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    s.slice(mark, i)
  }
}

/** Cursor for the `Iterator host`  */
class IteratorCursor[T](host: Iterator[T]) extends Cursor[T] {
  def hasCurrent: Boolean = host.hasNext
  var currentValue: Option[T] = None
  def current: T = currentValue match {
    case None =>
      val v=host.next()
      currentValue=Some(v)
      v
    case Some(v) => v
  }
  def next(): Unit = currentValue match {
    case None =>
      val v=host.next()
      currentValue=Some(v)
    case Some(v) =>
      currentValue=Some(host.next())
  }
}

/** Cursor for the `String s`  */
class StringCursor(s: String) extends Cursor[Char] {
  var i=0
  def hasCurrent: Boolean = i<s.length
  def current: Char = s(i)
  def next(): Unit = if (i<s.length) i+=1 else throw new IllegalStateException(s"StringCursor@$i>=${s.length}")
  override def takeWhile(p: Char=>Boolean):Seq[Char] = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    s.slice(mark, i)
  }

  /** Prefix of this cursor's remaining string all of whose characters satisfy `p` */
  def stringWhile(p: Char=>Boolean):String = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    s.slice(mark, i)
  }
}

class CharSequenceCursor(s: CharSequence) extends Cursor[Char] {
  var i=0
  def hasCurrent: Boolean = i<s.length
  def current: Char = s.charAt(i)
  def next(): Unit = if (i<s.length) i+=1 else throw new IllegalStateException(s"StringCursor@$i>=${s.length}")

  def CharSequenceWhile(p: Char=>Boolean): CharSequence = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    s.subSequence(mark, i)
  }

  /** Prefix of this cursor's remaining string all of whose characters satisfy `p` */
  def stringWhile(p: Char=>Boolean):String = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    val rep = new Array[Char](i-mark)
    var j   = mark
    for { i <- 0 until rep.length } {
      rep(i) = s.charAt(j)
      j += 1
    }
    new String(rep)
  }
}

/*
object TestCharSequenceCursor {

    import org.sufrin.utility.CharSequenceOperations._
    def main(args: Array[String]): Unit = {
      val l0 = constantSeq('=', 5)
      println(l0.toSeq)
      val l1  = "01234".leftJustify(10)
      val l1a = Cat(List("01234",l0))
      val l2 = leftJustify("01234", 10, '=')
      println(l2)
      val csc = new CharSequenceCursor(l2)
      println(csc.stringWhile(_.isDigit))
      println(csc.stringWhile(c=>true))

    }
}
*/

