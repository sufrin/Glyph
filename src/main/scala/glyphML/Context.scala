package org.sufrin.glyph
package glyphML


import org.sufrin.logging
import org.sufrin.logging.{Default, SourceDefault}
import org.sufrin.SourceLocation._
import org.sufrin.glyph.GlyphTypes.Scalar

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
    def mkString(sep: String = " "): String =
      (for { (k, d) <- map } yield s"$k=\"$d\"").mkString(sep)
  }

  /**
   *
   * An extension to provides type-specific interpretations of properties specified in an `AttributeMap`
   *
   */
  implicit class TypedAttributeMap(val attributes: AttributeMap) extends AnyVal {

    import logging.Default.warn

    /** Show as a mapping with pairs in the form `k="d"` */
    override def toString: String = attributes.map { case (k,d) => s"$k=\"$d\""}.mkString(" ")

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
     *  Evaluate an expression
     */
    def Eval(spec: String, alt: Float)(context: Context)(implicit at: SourceLocation = sourceLocation): Float = {
      val Rfactor = "(-?[0-9]+(\\.([0-9]+)?)?)(em|ex|px|pt|us)".r
      val RId     = "[a-zA-Z.]+".r
      val RNum    = "-?([0-9]+(\\.([0-9]+)?)?)".r
      import context.{sheet, definitions}
      def fetchGlyphDiagonal(id: String): Vec = {
        val stored = definitions.getKind(StoreType.GlyphConstant)(id)
        val result =
          stored match {
              case Some(StoredGlyphConstant(glyph)) =>
                glyph.diagonal
              case _ =>
                SourceDefault.warn(s"Unknown glyph: $id in ($spec)")(at)
                Vec.Zero
            }
        result
      }
      def evalGlobal(factor: Scalar, id: String): Scalar = {
        id match {
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
          case s"$id.width"  => factor*fetchGlyphDiagonal(id).x
          case s"$id.height" => factor*fetchGlyphDiagonal(id).y
          case other =>
            alt
        }
      }
      if (Rfactor.matches(spec))
        spec.toLowerCase match {
          case (s"${s}em") if RNum.matches(s) => s.toFloat * sheet.emWidth
          case (s"${s}ex") if RNum.matches(s) => s.toFloat * sheet.exHeight
          case (s"${s}px") if RNum.matches(s) => s.toFloat
          case (s"${s}pt") if RNum.matches(s) => s.toFloat
          case other =>
            SourceDefault.warn(s"$spec should specify its unit of measure in em/ex/px/pt, or as a <float>*(width/indent/leftmargin/rightmargin/windowwidth/windowheight).")(at)
            alt
        }
      else
        if (RId.matches(spec)) {
          evalGlobal(1f, spec)
        } else {
          spec match {
            case s"$multiplier*$global" if (RNum.matches(multiplier)) =>
              evalGlobal(multiplier.toFloat, global)
            case other =>
              SourceDefault.warn(s"Ill-formed expression $spec ")(at)
              alt
          }
        }
    }

    /**
     * Yields the Float with key `key` and value expressed as one of
     *
     * float(`em`|`ex`|`px`|`pt`|`ux`)
     *
     * float*symbolic
     *
     * symbolic
     *
     *
     * where symbolic is one of
     *
     * {{{
     *   width           |
     *   indent          |
     *   leftmargin      |
     *   rightmargin     |
     *   window.width    |
     *   window.height   |
     *   container.width |
     *   container.height|
     *   screen.width    |
     *   screen.height
     * }}}
     *
     * or
     *
     *  *gid*`.width | `*gid*`.height`
     *
     * where *gid* is the refid of a glyph, or something that has been "measured".
     */
    def Units(key: String, alt: Float)(context: Context)(implicit at: SourceLocation = sourceLocation): Float = {
      attributes.get(key.toLowerCase)  match {
        case Some(spec) =>
          Eval(spec, alt)(context)(at)
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

    /** Derive a new `StyleSheet` from `context.sheet`, using `attributes` */
    def deriveSheet(context: Context): StyleSheet = {
      val sheet = context.sheet
      val fontDetailSheet: StyleSheet =
        sheet
          .copy(
            fontScale       = Float("fontscale", 1.0f),

            textFontStyle   = FontFamily.styleNamed(String("textstyle", String("fontstyle", ""))),
            textFontFamily  = FontFamily(String("textfontfamily", String("fontfamily", sheet.textFontFamily.family))),
            textFontSize    = Float("textfontsize", Float("fontsize", sheet.textFontSize)),

            labelFontStyle  = FontFamily.styleNamed(String("labelstyle", String("fontstyle", ""))),
            labelFontFamily = FontFamily(String("labelfontfamily", String("fontfamily", sheet.labelFontFamily.family))),
            labelFontSize   = Float("labelfontsize", Float("fontsize", sheet.labelFontSize)),

            buttonFontStyle  = FontFamily.styleNamed(String("buttonstyle", String("fontstyle", ""))),
            buttonFontFamily = FontFamily(String("buttonfontfamily", String("fontfamily", sheet.buttonFontFamily.family))),
            buttonFontSize   = Float("buttonfontsize", Float("fontsize", sheet.buttonFontSize)),

            cdataFontStyle  = FontFamily.styleNamed(String("cdatastyle", String("fontstyle", ""))),
            cdataFontFamily = FontFamily(String("cdatafontfamily", String("fontfamily", sheet.cdataFontFamily.family))),
            cdataFontSize   = Float("fontsize", Float("cdatafontsize", sheet.cdataFontSize)),
            )

      val fontDetail = context.copy(sheet=fontDetailSheet)

      // Units are computed relative to the font details, which may have been redeclared
      fontDetailSheet.copy(
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
  case class Context(attributes: AttributeMap, sheet: StyleSheet, definitions: Definitions) {
    /**
     *  Yield a new `Env`  whose `attributes` are superseded by those
     *  specified by `localAttributes`, and whose `sheet` is derived
     *  from those attributes.
     */
    def updated(localAttributes: AttributeMap): Context = {
        val updatedAttributes = localAttributes supersede attributes
        Context(updatedAttributes, updatedAttributes.deriveSheet(this), definitions)
    }
  }

}


