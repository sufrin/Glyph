package org.sufrin
package utility

import org.sufrin.glyph.CodePointSeqMap.{CodePoint, CodePointSeq}
import org.sufrin.logging.{Loggable, WARN}

/**
 * A collection of text abbreviations and their translations. If `implicitUnicode` then
 * sequences of (up to 5) hex digits followed by "u+" or "\u" are treated as implicit
 * abbreviations.
 *
 * @param onLineTrigger abbreviations are triggered automatically
 * @param implicitUnicode unicode character encodings are implicit abbreviations for those characters
 *
 * TODO: Implement (prioritised) an API for named abbreviation tables that can be individually enabled.
 */

class TextAbbreviations(var onLineTrigger: Boolean = false, var implicitUnicode: Boolean=false, var onAmbiguous: (String, String, String, String) => Unit = TextAbbreviations.ambiguous) {
  import org.sufrin.glyph.CodePointSeqMap
  val forward: CodePointSeqMap[String] = new CodePointSeqMap[String]

  var reversible: Boolean = false
  val reverse: CodePointSeqMap[String] = new CodePointSeqMap[String]


  def clearMapping(): Unit = { forward.clear(); reverse.clear() }

  /** If no suffix of `chars` in `forward` matches some abbreviation return `None`; else return
   * `Some(t, l)`, where `t` is the translation of the longest such match, and `l` is
   * its length.
   */
  def findSubstitution(chars: CodePointSeq): Option[(String, Int)] = forward.longestSuffixMatch(chars, chars.length)
  /** If no suffix of `chars` in `reverse` matches some abbreviation return `None`; else return
   * `Some(t, l)`, where `t` is the translation of the longest such match, and `l` is
   * its length.
   */
  def findAbbreviation(chars: CodePointSeq): Option[(String, Int)] = reverse.longestSuffixMatch(chars, chars.length)


  def findImplicitUnicode(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] = if (implicitUnicode) {
      def isHex(cp: CodePoint): Boolean = '0'<=cp && cp<='9' || 'a'<=cp && cp <= 'f'
      def isSymbolic(c: CodePoint): Boolean = isHex(c) || c=='u' || c=='+' || c=='\\'
      def addHex(h: Int, cp: CodePoint):Int = (h<<4)+(if ('0'<=cp && cp<='9') cp-'0' else if ('a'<=cp && cp <= 'f') cp-'a'+10 else 0)
      def uniChar(hexits: List[CodePoint]): Option[(String, Int)] = {
        val codePoint: CodePoint = hexits.foldLeft(0)(addHex(_, _))
        Some(new String(Character.toChars(codePoint)), hexits.length+2)
      }
      val lowercase = reverseIterator().map(Character.toLowerCase(_))
      val symbol: List[CodePoint] = lowercase.takeWhile(isSymbolic(_)).take(7).toList
      //println(symbol.map(_.toChar))
      symbol match {
        //case '\\'::'u'::hexits if hexits.nonEmpty => uniChar(hexits)
        //case 'u'::'+'::hexits if hexits.nonEmpty  => uniChar(hexits)
        case 'u'::'u'::hexits if hexits.nonEmpty       => uniChar(hexits.reverse)
        case 'u'::'+'::hexits if hexits.nonEmpty       => uniChar(hexits.reverse)
        case '+'::'u'::hexits if hexits.nonEmpty       => uniChar(hexits.reverse)
        case _ => None
      }
    } else None

  /**
   * Equivalent to `findSubstitution(reverseIterator().toSeq)`
   */
  def reverseFindSubstitution(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] = {
    val firstTry = reverseIterator()
    val result = forward.longestPrefixMatch(firstTry)
    result orElse findImplicitUnicode(reverseIterator)
  }
  /**
   * Equivalent to `findAbbreviation(reverseIterator().toSeq)`
   */
  def reverseFindAbbreviation(reverseIterator: ()=>Iterator[CodePoint]): Option[(String, Int)] = {
    val firstTry = reverseIterator()
    val result = reverse.longestPrefixMatch(firstTry)
    result orElse findImplicitUnicode(reverseIterator)
  }


  import TextAbbreviations.toCodePoints

  def mapTo(abbrev: String, result: String): Unit = forward.reverseUpdate(toCodePoints(abbrev), result)

  def update(abbreviation: String, replacement: String)(implicit loc: String = SourceLocation.sourceLocation.toString): Unit = {
    forward.reverseChange(toCodePoints(abbreviation), replacement) match {
      case None =>
      case Some(oldReplacement) =>
        if (oldReplacement != replacement)
          onAmbiguous(abbreviation, oldReplacement, replacement, loc ++ " abbreviation")
    }

    if (reversible)
      reverse.reverseChange(toCodePoints(replacement), abbreviation) match {
      case None =>
      case Some(oldAbbreviation) =>
        if (oldAbbreviation != abbreviation)
          onAmbiguous(replacement, oldAbbreviation, abbreviation, loc ++ " unabbreviation")
    }


  }


}

object TextAbbreviations {

  def toCodePoints(str: String): CodePointSeq = {
    val a = new collection.mutable.ArrayBuffer[CodePoint]()
    str.codePoints.forEach { cp: Int => a.append(cp) }
    a.toSeq
  }

  def reportNewGlyph(glyph: String, codePoints: CodePointSeq): Unit = {
    logging.SourceDefault.info(s"New polyencoded glyph: $glyph ")
  }

  def ambiguous(abbr: String, oldRep: String, newRep: String, from: String): Unit = logging.SourceDefault.warn(s"$abbr was $oldRep now $newRep (from $from)")

  def decodeUnicode(cp: CodePoint): String = cp.toHexString+"uu"

}
