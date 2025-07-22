package org.sufrin
package utility

import org.sufrin.glyph.PrefixCodePointMap.{CodePoint, CodePointSequence}

class TextAbbreviations(var onLineTrigger: Boolean = false) {
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
  def reverseFindAbbreviation(reverseIterator: Iterator[CodePoint]): Option[(String, Int)] =
    trie.longestPrefixMatch(reverseIterator)

  def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(toCodePoints(abbrev), result)

  def update(abbreviation: String, replacement: String): Unit = trie.reverseUpdate(toCodePoints(abbreviation), replacement)

  def toCodePoints(str: String): CodePointSequence = {
    val a = new collection.mutable.ArrayBuffer[CodePoint]()
    str.codePoints.forEach { cp: Int => a.append(cp) }
    a.toSeq
  }
}
