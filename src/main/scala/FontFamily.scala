package org.sufrin.glyph

import GlyphTypes.{Font, FontManager, FontStyle, Scalar, Typeface}

import org.sufrin.glyph.FontFamily.{fontDescription, styleNamed}

object FontFamily extends org.sufrin.logging.Loggable {
  private val cache=collection.mutable.LinkedHashMap[(String, FontStyle, Scalar), Font]()
  def apply(name: String="Menlo"): FontFamily = new FontFamily(name)
  def fonts: Seq[String] = {
    for { (name, style, size) <- cache.keys.toSeq } yield s"$name::$style@$size"
  }

  def styleNamed(name: String): FontStyle =
    name.toLowerCase match {
      case "normal" => FontStyle.NORMAL
      case "bold" => FontStyle.BOLD
      case "italic" => FontStyle.ITALIC
      case "bolditalic" => FontStyle.BOLD_ITALIC
      case _ => FontStyle.NORMAL
    }

  private val fontDescription = collection.mutable.LinkedHashMap[Long, String]()
  def fontString(font: Font): String = fontDescription.getOrElse(font._ptr, font.toString)

  def styleString(fontStyle: FontStyle): String = {
    import FontStyle._
    fontStyle match {
      case NORMAL =>  "NORMAL"
      case BOLD => "BOLD"
      case ITALIC => "ITALIC"
      case BOLD_ITALIC => "BOLD_ITALIC"
      case _ => fontStyle.toString
    }
  }

}

/**
 * A font is a family in a style at a size.
 */
class FontFamily(val name: String) {
  override def toString: String = s"FontFamily($name)"
  lazy val normalFace:     Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.NORMAL)
  lazy val boldFace:       Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.BOLD)
  lazy val italicFace:     Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.ITALIC)
  lazy val boldItalicFace: Typeface = FontManager.default.matchFamilyStyle(name, FontStyle.BOLD_ITALIC)


  /**
   *  Yields the font specified by this family in `style` at `size`. A font is
   *  made only once, and thereafter cached.
   */
  def  makeFont(style: GlyphTypes.FontStyle=FontStyle.NORMAL, size: Scalar): Font = {
    val id=(name, style, size)
    FontFamily.cache.get(id) match {
      case Some(font) =>
        fontDescription(font._ptr) = s"$name($size, ${FontFamily.styleString(style)})"
        font

      case None =>
        val font = style match {
          case FontStyle.NORMAL => new Font(normalFace, size)
          case FontStyle.BOLD => new Font(boldFace, size)
          case FontStyle.ITALIC => new Font(italicFace, size)
          case FontStyle.BOLD_ITALIC =>
            new Font(boldItalicFace, size)
        }
        FontFamily.cache(id) = font
        FontFamily.info(s"new Font $id")
        font
    }
  }
  def apply(size: Scalar, style: String="normal"): Font = makeFont(style=styleNamed(style), size)
}