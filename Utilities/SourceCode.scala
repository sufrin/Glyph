package org.sufrin.utility

/**
 * Accumulate (source) text
 */
class SourceCode() {
  val s = new StringBuilder()
  @inline def out(line: String, nl: Boolean = true): Unit = {
    if (nl) s.append('\n')
    s.append(line)
  }
  @inline def gen(text: String): Unit = {
    s.append(text)
  }
  override def toString: String = s.toString
}