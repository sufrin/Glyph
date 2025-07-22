package org.sufrin
package utility

import org.sufrin.glyph.PrefixCodePointMap.{CodePoint, CodePointSequence}

class TextAbbreviations(var onLineTrigger: Boolean = false, var implicitUnicode: Boolean=false) {
  import org.sufrin.glyph.PrefixCodePointMap
  val trie: PrefixCodePointMap[String] = new PrefixCodePointMap[String]

  def clearMapping(): Unit = trie.clear()

  /**  */
  def findAbbreviation(chars: CodePointSequence, upTo: Int): Option[(String, Int)] =
    trie.longestSuffixMatch(chars, upTo)

  /**
   * Given an iterator traversing the (forward) sequence s in reverse,
   * find the longest suffix of `s` that matches a path from the root of `trie`.
   */
  def reverseFindAbbreviation(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] = {
    val firstTry = reverseIterator()
    val result = trie.longestPrefixMatch(firstTry)
    if (result.isDefined) result else
    if (implicitUnicode) {
        import org.sufrin.utility.CharSequenceOperations._
        def isHex(cp: CodePoint): Boolean = '0'<=cp && cp<='9' || 'a'<=cp && cp <= 'f' || cp=='u' || cp=='+'
        def addHex(h: Int, cp: CodePoint):Int =
          (h<<4)+(if ('0'<=cp && cp<='9') cp-'0' else if ('a'<=cp && cp <= 'f') cp-'a' else 0)
        val lastSeven = reverseIterator().takeWhile(cp=>isHex(Character.toLowerCase(cp))).take(7).toList.reverse
        lastSeven match {
          case 'u'::'+'::hexits =>
            if (hexits.length<5)  None else {
              val codePoint: CodePoint = hexits.foldLeft(0)(addHex(_, _))
              println(Character.toChars(codePoint).mkString)
              Some(Character.toChars(codePoint).mkString, 7)
            }
          case _ =>
            None
        }

      } else None
  }

  def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(toCodePoints(abbrev), result)

  def update(abbreviation: String, replacement: String): Unit = trie.reverseUpdate(toCodePoints(abbreviation), replacement)

  def toCodePoints(str: String): CodePointSequence = {
    val a = new collection.mutable.ArrayBuffer[CodePoint]()
    str.codePoints.forEach { cp: Int => a.append(cp) }
    a.toSeq
  }
}
