package org.sufrin.glyph

import GlyphTypes.{Font, FontManager, FontStyle, Scalar, Typeface}

object FontFamily extends org.sufrin.logging.Loggable {
  private val cache=collection.mutable.LinkedHashMap[(String, FontStyle, Scalar), Font]()
  def apply(name: String="Menlo"): FontFamily = new FontFamily(name)
  def fonts: Seq[String] =
    for { (name, style, size) <- cache.keys.toSeq } yield s"$name::$style@$size"
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
  def  makeFont(style: GlyphTypes.FontStyle, size: Scalar): Font = {
    val id=(name, style, size)
    FontFamily.cache.get(id) match {
      case Some(font) =>
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
  def apply(size: Scalar)(style: FontStyle): Font = makeFont(style, size)
  def apply(style:FontStyle)(size: Scalar): Font = makeFont(style, size)
}