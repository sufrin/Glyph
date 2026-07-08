package org.sufrin
package glyph

object NumberUtils {
  private val alph: Seq[String] =  ('A' to 'Z').map(_.toString)

  /** Lowercase hexavigesimal (see `Alpha`) */
  def alpha(num: Int): String = Alpha(num).toLowerCase

  /**
   * Hexavigesimal representation: digits A..Z represent 1 to 26, and (arbitrarily) "À" represents zero
   *
   * For example, if x, y, z, d are hexavigesimal digits representing X, Y, Z, D respectively
   * {{{
   * xd   represents X*26+D
   * xyd  represents X*(26*26)+(Y*26)+D
   * xyzd represents X*(26*26*26)+(Y*26*26)+Z*26+D
   * }}}
   *
   * More generally:
   * {{{when n>0, unAlpha(Alpha(n))==n}}}
   */
  def Alpha(num: Int): String = if (num==0) "À" else if (num<=26) alph(num-1) else Alpha((num-1) / 26) ++ alph((num - 1) % 26)

  /**
   * Hexavigesimal digit sequence to the integer it represents
   */
  def unAlpha(digits: String): Int = {
    def rec(rep: List[Char]): Int = if (rep.isEmpty) 0 else rep.head-'A'+1 + 26*rec(rep.tail)
    rec(digits.toList.reverse)
  }


  private val romanNumerals = List(
    100000 -> "C\u0305", // vinculum
    50000 -> "L\u0305",  // vinculum
    10000 -> "X\u0305",  // vinculum
    5000 -> "V\u0305",   // vinculum
    1000 -> "M",
    900 -> "CM",
    500 -> "D",
    400 -> "CD",
    100 -> "C",
    90 -> "XC",
    50 -> "L",
    40 -> "XL",
    10 -> "X",
    9 -> "IX",
    5 -> "V",
    4 -> "IV",
    1 -> "I"
  )

  def roman(num: Int): String = Roman(num).toLowerCase
  def Roman(num: Int): String = {
    assert(0<=num && num<600000, "Number out of representable range (1-600000)")
    var rep = ""
    var n = num
    for ((value, numeral) <- romanNumerals) {
      while (n >= value) {
        rep = s"$rep$numeral"
        n -= value
      }
    }
    rep
  }

  import org.sufrin.utility.CharSequenceOperations._

  /**
   *   Pre: `s` consists only of hexadecimal characters
   *   @return the integer represented in hexadecimal by the string `s`.
   */
  def hexToInt(s: String): Int = {
    //s.toLowerCase.toList.map("0123456789abcdef".indexOf(_)).reduce (_ * 16 + _) // { (l,d) => (l * 16 + d)}
    s.hexToInt.get
  }

  /**
   * @param s  \uxxxx (where the x are hex digits)
   * @return the Char whose hexadecimal representation is xxxx
   */
  def toUnicode(s: String): Char = s.toUnicode.get

  /**
   *
   * @param s up to 8 hex digits
   * @return the Long whose representation is s
   */
  def hexToLong(s: String): Long = s.hexToLong.get

}
