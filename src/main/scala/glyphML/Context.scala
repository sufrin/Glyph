package org.sufrin.glyph
package glyphML


import org.sufrin.logging
import org.sufrin.logging.Default
import org.sufrin.logging.Default.warn
import org.sufrin.SourceLocation._

object Context {
  type AttributeMap = Map[String, String]

  private def SUPERSEDE(l: AttributeMap, r: AttributeMap): AttributeMap = new AttributeMap {

    def removed(key: String): Map[String, String] = SUPERSEDE(l.removed(key), r.removed(key))

    def removedAll(keys: String*): Map[String, String] = SUPERSEDE(l.removedAll(keys), r.removedAll(keys))

    def get(key: String): Option[String] = l.get(key) orElse (r.get(key))

    override def contains(d: String): Boolean = l.contains(d) || r.contains(d)

    override def keysIterator: Iterator[String] =
      l.keysIterator.concat(r.keysIterator.filterNot(l.contains(_)))

    def iterator: Iterator[(String, String)] =
      l.iterator.concat(r.filterNot { case (d, r) => l.contains(d) })

    def updated[V1 >: String](key: String, value: V1): Map[String, V1] = updated(key, value.asInstanceOf[String])

    def updated(key: String, value: String): AttributeMap = Map((key, value)).supersede(this)
  }

  implicit class ExtendedAttributeMap(val map: AttributeMap) extends AnyVal {
    def supersede(r: AttributeMap): AttributeMap = SUPERSEDE(map, r)
    def without(keys: String*): AttributeMap = map.removedAll(keys)
  }

  implicit class TypedAttributeMap(val attributes: AttributeMap) extends AnyVal {

    import logging.Default.warn

    /** Show as a mapping with pairs in the form `k->d` */
    override def toString: String = attributes.map { case (k,d) => s"$k->$d"}.mkString(", ")

    /**
     * Yields the string with key `key`, or `alt`
     */
    def String(key: String, alt: String): String = attributes.getOrElse(key.toLowerCase, alt)

    /**
     * Yields the Int with key `key` (if it looks like a number), or `alt`
     */
    def Int(key: String, alt: Int)(implicit at: SourceLocation = sourceLocation): Int = attributes.get(key.toLowerCase) match {
      case Some(s) if s.matches("-?[0-9]+") => s.toInt
      case Some(s) =>
        warn(s"$key(=$s) should be an Int [using $alt] ($at)")
        alt
      case None => alt
    }

    /**
     * Yields Float Int with key `key` (if it looks like a floating point number), or `alt`
     */
    def Float(key: String, alt: Float)(implicit at: SourceLocation = sourceLocation): Float = attributes.get(key.toLowerCase) match {
      case Some(spec) =>
        try {
          spec.toFloat
        }
        catch {
          case exn: Throwable => Default.warn(s"$key(=$spec) should be a Float [using $alt] ($at)")
            alt
        }
      case None => alt
    }

    /**
     * Yields the Float with key `key` if is expressed in one of the following forms:
     *
     * 1. A floating point number, `factor`, immediately followed by one of
     * {{{
     *  em => factor*sheet.emWidth
     *  ex => factor*sheet.exHeight
     * }}}
     *
     * {{{
     *   px => factor
     *   pt => factor
     * }}}
     *
     * 2. a floating point factor, `factor`, immediately followed by `*` then by one of the following:
     * {{{
     *    width => factor*sheet.parWidth
     *    indent => factor*sheet.parIndent
     *    leftmargin => factor*sheet.leftMargin
     *    rightmargin => factor*sheet.rightMargin
     *    container.width => factor*sheet.containerWidth
     *    container.height => factor*sheet.containerHeight
     *    window.width => factor*sheet.windowWidth
     *    window.height => factor*sheet.windowHeight
     *    screen.width => factor*sheet.screenWidth
     *    screen.height => factor*sheet.screenHeight
     * }}}
     *
     */
    def Units(key: String, alt: Float)(sheet: StyleSheet)(implicit at: SourceLocation = sourceLocation): Float = {
      attributes.get(key.toLowerCase) match {
        case Some(spec) =>
          spec.toLowerCase match {
            case (s"${s}em") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * sheet.emWidth
            case (s"${s}ex") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * sheet.exHeight
            case (s"${s}px") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
            case (s"${s}pt") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
            case (s"${m}*${dim}") if m.isEmpty || m.matches("[0-9]+(\\.([0-9]+)?)?") =>
              val factor = if (m.isEmpty) 1f else m.toFloat
              dim.toLowerCase match {
                case "width" => factor * sheet.parWidth
                case "indent" => factor * sheet.parIndent
                case "leftmargin" => factor * sheet.leftMargin
                case "rightmargin" => factor * sheet.rightMargin
                case "container.width" => factor * sheet.containerWidth
                case "container.height" => factor * sheet.containerHeight
                case "window.width" => factor * sheet.windowWidth
                case "window.height" => factor * sheet.windowHeight
                case "screen.width" => factor * sheet.screenWidth
                case "screen.height" => factor * sheet.screenHeight
                case other =>
                  warn(s"$key(=$other) should specify its unit of measure in em/ex/px/pt, or as a <float>*(width/indent/leftmargin/rightmargin/windowwidth/windowheight). ($at)")
                  alt
              }
            case (other) =>
              warn(s"$key(=$other) should specify its unit of measure in em/ex/px/pt, or as a fractional multiple of width/indent/leftmargin/rightmargin/etc. ($at)")
              alt
          }
        case None =>
          alt
      }
    }

    /**
     * Yields the Boolean with key `key` (if it's t, f, true, false, on, or off), or `default`
     */
    def Bool(key: String, default: Boolean)(implicit at: SourceLocation = sourceLocation): Boolean = attributes.get(key.toLowerCase) match {
      case None => default
      case Some(boolean) =>
        boolean.toLowerCase match {
          case "t" | "true" | "on" => true
          case "f" | "false" | "off" => false
          case _ =>
            warn(s"$key=\"$boolean\" should be t/f/true/on/false/off ($at)")
            default
        }
    }

    /**
     * Yields the lateral alignment with key `key` (if it's one of `{left, right, center, justify}`), or `default`
     */
    def Align(key: String, default: Alignment)(implicit at: SourceLocation = sourceLocation): Alignment = attributes.get(key.toLowerCase) match {
      case None => default
      case Some(alignment) => alignment.toLowerCase match {
        case ("left") => Left
        case ("right") => Right
        case ("center") => Center
        case ("justify") => Justify
        case (other) =>
          warn(s"$key=\"$other\" [not a lateral alignment name: using \"center\"] ($at)")
          Center
      }
    }


    /**
     * Yields the vertical alignment with key `key` (if it's one of `{top, bottom, mid, center}`), or `default`
     */
    def VAlign(key: String, default: VAlignment)(implicit at: SourceLocation = sourceLocation): VAlignment = attributes.get(key.toLowerCase) match {
      case None => default
      case Some(alignment) => alignment.toLowerCase match {
        case ("top") => Top
        case ("bottom") => Bottom
        case ("mid") => Mid
        case ("center") => Mid
        case (other) =>
          warn(s"$key=\"$other\" [not a vertical alignment name: using \"mid\" ($at)]")
          Mid
      }
    }


    /**
     * Yields the brush whose `Brushes` specification has key `key`, or `alt`.
     *
     * @see Brushes
     */
    def Brush(key: String, alt: Brush): Brush = attributes.get(key.toLowerCase) match {
      case None => alt
      case Some(name) => Brushes(name)
    }

    /** Derive a new `StyleSheet` from `sheet`, using `attributes` */
    def deriveSheet(sheet: StyleSheet): StyleSheet = {
      val fontDetail: StyleSheet =
        sheet
          .copy(
            fontScale       = Float("fontscale", 1.0f),

            textFontStyle   = FontFamily.styleNamed(String("textstyle", "")),
            textFontFamily  = FontFamily(String("fontfamily", String("textfontfamily", sheet.textFontFamily.family))),
            textFontSize    = Float("fontsize", Float("textfontsize", sheet.textFontSize)),

            labelFontStyle  = FontFamily.styleNamed(String("labelstyle", "")),
            labelFontFamily = FontFamily(String("fontfamily", String("labelfontfamily", sheet.labelFontFamily.family))),
            labelFontSize   = Float("fontsize", Float("labelfontsize", sheet.labelFontSize)),

            buttonFontStyle  = FontFamily.styleNamed(String("buttonstyle", "")),
            buttonFontFamily = FontFamily(String("fontfamily", String("buttonfontfamily", sheet.buttonFontFamily.family))),
            buttonFontSize   = Float("fontsize", Float("buttonfontsize", sheet.buttonFontSize)),

            cdataFontStyle  = FontFamily.styleNamed(String("cdatastyle", "")),
            cdataFontFamily = FontFamily(String("fontfamily", String("cdatafontfamily", sheet.cdataFontFamily.family))),
            cdataFontSize   = Float("fontsize", Float("cdatafontsize", sheet.cdataFontSize)),
            )

      // Units are computed relative to the font details, which may have been redeclared
      fontDetail.copy(
        padX                  = Units("padx",           sheet.padX)         (fontDetail),
        padY                  = Units("pady",           sheet.padY)         (fontDetail),
        parWidth              = Units("width",          sheet.parWidth)     (fontDetail),
        parSkip               = Units("parskip",        sheet.parSkip)      (fontDetail),
        parIndent             = Units("parindent",      sheet.parIndent)    (fontDetail),
        leftMargin            = Units("leftmargin",     sheet.leftMargin)   (fontDetail),
        rightMargin           = Units("rightmargin",    sheet.rightMargin)  (fontDetail),
        parAlign              = Align("align",          sheet.parAlign),
        backgroundBrush       = Brush("background",     sheet.backgroundBrush),
        foregroundBrush       = Brush("foreground",     sheet.foregroundBrush),
        textBackgroundBrush   = Brush("textbackground", sheet.textBackgroundBrush),
        textForegroundBrush   = Brush("textforeground", sheet.textForegroundBrush),
        buttonBackgroundBrush = Brush("buttonbackground", sheet.buttonBackgroundBrush),
        buttonForegroundBrush = Brush("buttonforeground", sheet.buttonForegroundBrush),
        labelBackgroundBrush  = Brush("labelbackground", sheet.labelBackgroundBrush),
        labelForegroundBrush  = Brush("labelforeground", sheet.labelForegroundBrush),
        cdataBackgroundBrush  = Brush("cdatabackground", sheet.cdataBackgroundBrush),
        cdataForegroundBrush  = Brush("cdataforeground", sheet.cdataForegroundBrush),
        )
    }

  }

  /**  */
  case class Env(attributes: AttributeMap, sheet: StyleSheet) {
    /**
     *  Yield a new `Env`  whose `attributes` are superseded by those
     *  specified by `localAttributes`, and whose `sheet` derived
     *  from those attributes.
     */
    def updated(localAttributes: AttributeMap): Env = {
        val updatedAttributes = localAttributes supersede attributes
        Env(updatedAttributes, updatedAttributes.deriveSheet(sheet))
    }
  }

}


