package org.sufrin
package glyph

import org.sufrin.glyph.FontFamily.{styleNamed, styleString}
import org.sufrin.glyph.GlyphTypes.{Font, FontManager, FontStyle, Scalar, Typeface}

/**
 *  Unique identifier for a font
 */
case class FontID(family: String, style: FontStyle, size: Scalar) {
  override val toString: String = s"$family::${styleString(style)}@$size"
}

/**
 *  Font construction API.
 *
 *  `Font`s and `FontFamily`s made with this API are cached in the `fontStore`,
 *  and `familyStore` respectively. Font families are lightweight objects.
 *
 *  The API has two main  ways of building fonts.
 *
 *  {{{
 *    FontFamily)(name).makeFont(style, size)
 *    FontFamily(FontID(name, style, size))
 *    FontFamily(name, style, size)
 *  }}}
 *
 */
object FontFamily extends logging.Loggable {

  private val fontStore=collection.mutable.LinkedHashMap[FontID, Font]()
  private val familyStore=collection.mutable.LinkedHashMap[String, FontFamily]()

  /** Build and cache a font family */
  def apply(family: String="Menlo"): FontFamily =
      familyStore.getOrElseUpdate(family, new FontFamily(family))

  /** Build and cache a Font with properties specified by the given `FontId` */
  def apply(fontID: FontID): Font = {
    val FontID(family, style, size) = fontID
    FontFamily.apply(family).makeFont(style, size)
  }

  /** Build and cache a Font with the given properties */
  def apply(family: String, style: FontStyle, size: Scalar): Font = {
    FontFamily.apply(family).makeFont(style, size)
  }

  def apply(family: String, style: String, size: Scalar): Font = {
    FontFamily.apply(family).makeFont(styleNamed(style), size)
  }

  /** Names of all the font families in use */
  def families: Seq[String] = familyStore.keys.toSeq

  /** Name strings of all the fonts in use */
  def fonts: Seq[String] = {
    for { id <- fontStore.keys.toSeq } yield id.toString
  }

  /** Map the name of a standard fontstyles to the corresponding style. */
  def styleNamed(name: String): FontStyle =
    name.toLowerCase match {
      case "normal" => FontStyle.NORMAL
      case "bold" => FontStyle.BOLD
      case "italic" => FontStyle.ITALIC
      case "bolditalic" => FontStyle.BOLD_ITALIC
      case _ => FontStyle.NORMAL
    }

  /**
   * The `FontID` of the given `font`, if it was
   * constructed using the `FontFamily` API.
   */
  def fontID(font: Font): Option[FontID] = {
    var id: Option[FontID] = None
    val fonts = fontStore.iterator
    while (fonts.hasNext && id.isEmpty) {
      val (fontID, _font) = fonts.next()
      if (font eq _font) id = Some(fontID)
    }
    id
  }

  /**
   * A string denoting the given `font`, if it was
   * constructed using the `FontFamily` API; otherwise
   * a more-or-less inscrutable Java object referring
   * to the font's internal representation.
   */
  def fontString(font: Font): String = {
       fontID(font) match {
         case None => font.toString
         case Some(id) => id.toString
    }
  }

  /**
   * @param fontStyle
   * @return the name of the given `FontStyle`
   */
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

  /**
   * Extended API
   * @param font
   */
  implicit class Extensions(val font: Font) extends AnyVal {
    def asString: String = fontString(font)
    /**
     * This `font` at `scale*` its current size
     */
    def scaled(scale: Scalar): Font = {
      FontFamily.fontID(font) match {
        case Some(id) => FontFamily(id.family, id.style, id.size * scale)
        case None     => font
      }
    }

    /**
     * This `font` at `size`
     */
    def sized(size: Scalar): Font = {
      val id = FontFamily.fontID(font)
      id match {
        case Some(id) => FontFamily(id.family, id.style, size)
        case None     => font
      }
    }

    /**
     * This `font` in the named style (if possible)
     */
    def styled(style: String): Font = {
      FontFamily.fontID(font) match {
        case Some(id) => FontFamily(id.family, FontFamily.styleNamed(style), id.size)
        case None     => font
      }
    }

    def familied(family: String): Font = {
      FontFamily.fontID(font) match {
        case Some(id) => FontFamily(family, id.style, id.size)
        case None     => font
      }
    }
  }

}

/**
 * A font is from a family in a style at a size.
 */
class FontFamily(val family: String) {
  override def toString: String = s"FontFamily($family)"
  lazy val normalFace:     Typeface = FontManager.default.matchFamilyStyle(family, FontStyle.NORMAL)
  lazy val boldFace:       Typeface = FontManager.default.matchFamilyStyle(family, FontStyle.BOLD)
  lazy val italicFace:     Typeface = FontManager.default.matchFamilyStyle(family, FontStyle.ITALIC)
  lazy val boldItalicFace: Typeface = FontManager.default.matchFamilyStyle(family, FontStyle.BOLD_ITALIC)

  /**
   *  Yields the font specified by this family in `style` at `size`. A font is
   *  made only once, and thereafter cached in the font store.
   *
   * @see FontFamily.apply
   */
  def  makeFont(style: GlyphTypes.FontStyle=FontStyle.NORMAL, size: Scalar): Font = {
    val id=FontID(family, style, size)
    FontFamily.fontStore.get(id) match {
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
        FontFamily.fontStore(id) = font
        FontFamily.info(s"new Font $id")
        font
    }
  }

  /**
   *    * @return the font from this family in the given style, at the given size.
   */
  def apply(size: Scalar, style: String="normal"): Font = makeFont(style=styleNamed(style), size)
}