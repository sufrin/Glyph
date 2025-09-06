package org.sufrin.glyph
package utility

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

  def stringWhile(p: Char=>Boolean):String = {
    val mark = i
    while (hasCurrent && p(current)) {
      next()
    }
    s.slice(mark, i)
  }
}

trait Cursor[T] { hostCursor =>
  def hasCurrent: Boolean
  def current: T
  def next(): Unit

  def takeWhile(p: T=>Boolean):Seq[T] = {
    var r: List[T] = Nil
    while (hasCurrent && p(current)) {
      r = current :: r
      next()
    }
    r.reverse
  }

  def dropWhile(p: T=>Boolean): Unit = {
    while (hasCurrent && p(current)) {
      next()
    }
  }

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

  def iterator: Iterator[T] =  new Iterator[T] {
    def hasNext: Boolean = hostCursor.hasCurrent

    def next(): T = {
      val v = hostCursor.current
      hostCursor.next()
      v
    }
  }

}

