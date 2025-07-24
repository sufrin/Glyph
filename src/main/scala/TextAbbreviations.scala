package org.sufrin
package utility

import org.sufrin.glyph.PrefixCodePointMap.{CodePoint, CodePointSequence}

/**
 * A collection of text abbreviations and their translations. If `implicitUnicode` then
 * sequences of (up to 5) hex digits followed by "u+" or "\u" are treated as implicit
 * abbreviations.
 *
 * @param onLineTrigger abbreviations are triggered automatically
 * @param implicitUnicode unicode character encodings are implicit abbreviations for those characters
 */

class TextAbbreviations(var onLineTrigger: Boolean = false, var implicitUnicode: Boolean=false) {
  import org.sufrin.glyph.PrefixCodePointMap
  val trie: PrefixCodePointMap[String] = new PrefixCodePointMap[String]

  def clearMapping(): Unit = trie.clear()

  /** If no suffix of `chars` matches some abbreviation return `None`; else return
   * `Some(t, l)`, where `t` is the translation of the longest such match, and `l` is
   * its length.
   */
  def findAbbreviation(chars: CodePointSequence): Option[(String, Int)] =
    trie.longestSuffixMatch(chars, chars.length) orElse findImplicitUnicode(()=>chars.reverseIterator)

  def findImplicitUnicode(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] =
    if (implicitUnicode) {
      def isHex(cp: CodePoint): Boolean = '0'<=cp && cp<='9' || 'a'<=cp && cp <= 'f' || cp=='u' || cp=='+' || cp== '\\'
      def addHex(h: Int, cp: CodePoint):Int = (h<<4)+(if ('0'<=cp && cp<='9') cp-'0' else if ('a'<=cp && cp <= 'f') cp-'a'+10 else 0)
      def uniChar(hexits: List[CodePoint]): Option[(String, Int)] = {
        val codePoint: CodePoint = hexits.foldLeft(0)(addHex(_, _))
        Some(new String(Character.toChars(codePoint)), hexits.length+2)
      }
      val lastSeven: List[CodePoint] = reverseIterator().map(Character.toLowerCase(_)).takeWhile(isHex(_)).take(7).toList
      lastSeven match {
        case 'u'::'\\'::hexits => uniChar(hexits.reverse)
        case '+'::'u'::hexits  => uniChar(hexits.reverse)
        case _ => None
      }
    } else None

  /**
   * Equivalent to `findAbbreviation(reverseIterator().toSeq)`
   */
  def reverseFindAbbreviation(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] = {
    val firstTry = reverseIterator()
    val result = trie.longestPrefixMatch(firstTry)
    result orElse findImplicitUnicode(reverseIterator)
  }

  import TextAbbreviations.toCodePoints

  def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(toCodePoints(abbrev), result)

  def update(abbreviation: String, replacement: String): Unit = trie.reverseUpdate(toCodePoints(abbreviation), replacement)


}

object TextAbbreviations {
  def toCodePoints(str: String): CodePointSequence = {
    val a = new collection.mutable.ArrayBuffer[CodePoint]()
    str.codePoints.forEach { cp: Int => a.append(cp) }
    a.toSeq
  }
}
