package org.sufrin.utility

class TextAbbreviations(var onLineTrigger: Boolean = false) {
  val trie: PrefixMap[String] = new PrefixMap[String]

  def clearMapping(): Unit = trie.clear()

  def findAbbreviation(chars: CharSequence, upTo: Int): Option[(String, Int)] =
    trie.longestSuffixMatch(chars, upTo)

  def mapTo(abbrev: String, result: String): Unit = trie.reverseUpdate(abbrev, result)

  def update(abbreviation: String, replacement: String): Unit = trie.reverseUpdate(abbreviation, replacement)
}
