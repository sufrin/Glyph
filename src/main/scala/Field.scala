package org.sufrin.util

/**
 * Formatting methods for Long values.
 */
object Field {

  /** Signed decimal representation of `n` */
  def dec(n: Long): String = n.toString

  @inline private def log10(n: Long): Int = {
    var l = 0
    var v = n
    while (v!=0) { l+=1; v/=10 }
    l-1
  }

  @inline private def exp10(n: Int): Long = {
    // requires n>=0
    var r: Long=1
    var c=n
    while (c!=0) { r *= 10; c -= 1}
    r
  }

  /** Decimal representation of `n` with `precision` digits after the decimal point.*/
  def decimal(precision: Int)(n: Long): String = {
    // requires precision>=0
    val modulus=exp10(precision)
    val pad="0"*precision
    dec(n / modulus)+"."+atRight(pad, dec)((n%modulus)*n.sign)
  }

  /** Minimal-width hexadecimal representation of `n` */
  def hex(n: Long): String = n.toHexString

  /** Minimal-width octal representation of `n` */
  def oct(n: Long): String = n.toOctalString

  /** Minimal-width binary representation of `n` */
  def bin(n: Long): String = n.toBinaryString

  /**
   * @return `asString(n)` at the left of a field containing `padding`
   */
  def atLeft(padding: String, asString: Long=>String)(n: Long): String = {
    val string = asString(n)
    val pad = padding.subSequence(string.length min padding.length, padding.length)
    s"$string$pad"
  }

  /**
   * @return `asString(n)` at the right of a field containing `padding`
   */
  def atRight(padding: String, asString: Long=>String)(n: Long): String = {
    val string = asString(n)
    val pad = padding.subSequence(0, (padding.length-string.length) max 0)
    s"$pad$string"
  }

  private val pads: Array[String] = new Array[String](30)

  def pad(width: Int): String = synchronized {
    if (width<pads.length && (pads(width) eq null)) pads(width) = " "*width
    pads(width)
  }

  def leftJustify(width: Int, asString: Long=>String): Long=>String = {
    val padding = pad(width)
    atLeft(padding, asString)
  }

  def rightJustify(width: Int, asString: Long=>String): Long=>String = {
    val padding = pad(width)
    atRight(padding, asString)
  }

  /**
   * @param width width of the field if `width.abs` (negative => left justified)
   * @param symbol currency symbol
   * @param cr positive symbol
   * @param dr negative symbol
   * @param pad padding between currency symbol and  number
   * @param hundredths number denominated in hundredths
   * @return String representation of length `width.abs` of an amount denominated in hundredths of a currency unit and decorated with currency symbol, cr, dr
   *
   * The field width should be large enough to accommodate the largest expected amount
   */
  def currencyField(width: Int, symbol: String, cr: String="+", dr: String="-", pad: String="_")(hundredths: Long): String = {
    val w = if (width<0) -width else width
    val (n, prefix): (Long, String) = if (hundredths<0) (-hundredths, dr) else (hundredths, cr)
    val padding = pad*(w-symbol.length-prefix.length)
    val res = width.sign match {
      case 0 | 1 => atRight(prefix+symbol+padding, decimal(100))(n)
      case -1    => prefix+symbol+atLeft(padding, decimal(100))(n)
    }
    assert(res.length==w, s"""Inadequate specification: currencyField($width, "$symbol", "$cr", "$dr", "$pad")($hundredths) """)
    res
  }

}
