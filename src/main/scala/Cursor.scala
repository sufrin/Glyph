package org.sufrin.glyph
package utility

class Cursor[T](s: Seq[T]) {
  var i=0
  def hasCurrent: Boolean = i<s.length
  def current: T = s(i)
  def next(): Unit = if (i<s.length) i+=1 else throw new IllegalStateException(s"SeqCursor@$i>=${s.length}")
  def takeWhile(p: T=>Boolean):Seq[T] = {
    var r: List[T] = Nil
    while (hasCurrent && p(current)) {
      r = current :: r
      next()
    }
    r.reverse
  }
}
