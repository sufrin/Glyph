package org.sufrin.glyph

import ReactiveGlyphs.{ColourButton, Reaction}

object XML {
  val TOPLEVEL = List("")
  /** A column described by the given xml node translated in the default context using the given style sheet.  */
  def apply(node: xml.Node)(implicit sheet: StyleSheet): Glyph = {
    val local = GlyphML.Context(style=sheet)
    val glyphs: Seq[Glyph] = GlyphXML.translate(TOPLEVEL)(node)(Map.empty)(local)
    NaturalSize.Col().centered$(glyphs)
  }

  /** A column described by the given xml node translated in the given local context.  */
  def Col(node: xml.Node)(implicit local: GlyphML.Context): Glyph = {
    val glyphs: Seq[Glyph] = GlyphXML.translate(TOPLEVEL)(node)(Map.empty)(local)
    NaturalSize.Col().centered$(glyphs)
  }

  implicit def XMLtoGlyph(node: xml.Node)(implicit local: GlyphML.Context): Glyph = Col(node)(local)

  /** A row described by the given xml node translated in the given local context.  */
  def Row(node: xml.Node)(implicit local: GlyphML.Context): Glyph = {
    val glyphs: Seq[Glyph] = GlyphXML.translate(TOPLEVEL)(node)(Map.empty)(local)
    NaturalSize.Col().centered$(glyphs)
  }

}

object GlyphXML extends org.sufrin.logging.SourceLoggable {

  import GlyphML.Context

  import scala.xml._

  type AttributeMap = Map[String,String]

  implicit class TypedAttributeMap(attributes: AttributeMap) {

    def String(key: String, alt: String): String = attributes.getOrElse(key, alt)

    def Int(key: String, alt: Int): Int = attributes.get(key) match {
      case Some (s) if s.matches("-?[0-9]+") => s.toInt
      case Some(s)  =>
        warn(s"$key(=$s) should be an Int [using $alt]")(source)
        alt
      case None     => alt
    }

    def Float(key: String, alt: Float): Float = attributes.get(key) match {
      case Some (s) =>
        try { s.toFloat }
        catch {
          case exn: Throwable  => warn(s"$key(=$s) should be a Float [using $alt]")(source)
          alt
        }
      case None     => alt
    }

    def Reaction(key: String, alt: Reaction)(context: GlyphML.Context): Reaction = attributes.get(key) match {
      case Some (reactionName) =>
           context.reactionMap.getOrElse(reactionName, { _ => })
      case None =>
        alt
    }

    def Units(key: String, alt: Float)(context: GlyphML.Context): Float = attributes.get(key) match {
      case Some(spec) =>
        spec.toLowerCase match {
          case (s"${s}em") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * context.emWidth
          case (s"${s}ex") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * context.exHeight
          case (s"${s}px") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
          case (s"${s}pt") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
          case (other) =>
            warn(s"$key(=$other) should specify its unit of measure in em/ex/px/pt")(source)
            alt
      }
      case None =>
        alt
    }

    def Bool(key: String, default: Boolean): Boolean = attributes.get(key.toLowerCase) match {
      case None => default
      case Some(boolean) =>
        boolean.toLowerCase match {
          case "t" | "true"  | "on" => true
          case "f" | "false" | "off" => false
          case _ =>
            warn(s"$key=\"$boolean\" should be t/f/true/on/false/off")
            default
        }
    }

    def Align(key: String, default: Alignment): Alignment = attributes.get(key) match {
      case Some("left") => Left
      case Some("right") => Right
      case Some("center") => Center
      case Some("justify") => Justify
      case Some(other) =>
        warn(s"$key=\"$other\" [not an alignment name: using \"center\"]")
        Center
      case None =>
        default
    }

    import org.sufrin.glyph.Brush.ROUND

    // TODO: better notation for brushes
    def Brush(key: String, alt: Brush): Brush = attributes.get(key) match {
      case None       => alt
      case Some(s"0X$hex") if hex.matches("[0-9A-F][0-9A-F][0-9A-F][0-9A-F]") =>
        org.sufrin.glyph.Brush(s"0X$hex").color(hexToInt(hex))
      case Some(name) => name.toLowerCase match {
        case "red"  => org.sufrin.glyph.Brush(s"red").color(0XFFFF0000)
        case "blue" => org.sufrin.glyph.Brush(s"blue").color(0XFF0000FF)
        case "green" => org.sufrin.glyph.Brush(s"blue").color(0XFF00FF00)
        case "lightgrey" => DefaultBrushes.lightGrey
        case "darkgrey" => DefaultBrushes.darkGrey
        case "yellow" => DefaultBrushes.yellow
        case "red4"  => org.sufrin.glyph.Brush(s"red").color(0XFFFF0000).strokeWidth(6).strokeCap(ROUND)
        case "blue4" => org.sufrin.glyph.Brush(s"blue").color(0XFF0000FF).strokeWidth(6).strokeCap(ROUND)
        case "green4" => org.sufrin.glyph.Brush(s"blue").color(0XFF00FF00).strokeWidth(6).strokeCap(ROUND)
        case "lightgrey4" => DefaultBrushes.lightGrey.strokeWidth(6).strokeCap(ROUND)
        case "darkgrey4" => DefaultBrushes.darkGrey.strokeWidth(6).strokeCap(ROUND)
        case "yellow4" => DefaultBrushes.yellow.strokeWidth(6).strokeCap(ROUND)
        case "nothing" => DefaultBrushes.nothing
        case _ => DefaultBrushes.black
      }
    }

    /**
     * Context derived from `context` by declaring
     * the universally-applicable attributes.
     */
    def declareAttributes(context: Context): Context =
      context
      .copy(parAlign=Align("align", context.parAlign),
        fontFamily=FontFamily(String("fontFamily", context.fontFamily.name)),
        fontSize=Float("fontSize", context.fontSize),
        paragraphWidth=Units("width", context.paragraphWidth)(context),
        fg=Brush("fg", DefaultBrushes.black),
        bg=Brush("bg", DefaultBrushes.nothing),
        leftMargin = Float("leftMargin",   context.leftMargin),
        rightMargin = Float("rightMargin", context.leftMargin),
        parSkip = Units("parSkip", 0f)(context)
      )
      .indentEms(Int("l", 0), Int("r", 0))
      .fontScale(Float("fontScale", 1.0f))
  }

  private val stylingTags: Seq[String] = List("b", "em", "i", "indent", "bi", "n")
  /**
   * 1. Translate `<tag attrs>body</tag>` into a glyph sequence. The `context`  provides
   * glyph-specific features, and appropriate aspects of the context are
   * defined locally by attributes specific to each `tag`.
   *
   * In general, the `body` is translated with an inherited attribute mapping derived by extending/overriding
   * `inherited` by the mapping described by `attrs`.
   *
   * 2. Each individual text is translated by splitting it into words at space, newline and tab boundaries, then
   * translating each word into a glyph in `context.font`.
   *
   */
  def translate(within: List[String])(elem: Node)(inherited: AttributeMap)(context: GlyphML.Context): Seq[Glyph] = {
      val localAttributes = elem.attributes.asAttrMap
      val attributes = inherited ++ localAttributes
      val within$ = elem.label :: within
      def stylingTag(tag: String): Boolean = stylingTags.contains(tag)
      val inPara: Boolean =
        within.dropWhile(stylingTag).head=="p"


      @inline def toGlyph(word: org.sufrin.glyph.Text): Glyph =
        if (inPara) word.atBaseline() else word.asGlyph()


      def framed(glyph: Glyph): Glyph = {
          val brush = attributes.Brush("framed", DefaultBrushes.nothing)
          if (brush ==  DefaultBrushes.nothing) glyph else glyph.framed(brush)
      }


      elem match {
        case Text(buffer) =>
          import context.font
          import org.sufrin.glyph.{Text => TextChunk}
          val fg = context.fg
          val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
          buffer.toString.split("[\n\t ]+").toSeq.filterNot(_.isBlank).map{ word => toGlyph(TextChunk(word, font, fg, bg)) }

        case PCData(text) =>
          import context.font
          import org.sufrin.glyph.{Text => TextChunk}
          val fg = context.fg
          val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
          val glyphs = text.split('\n').toSeq.map(TextChunk(_, font, fg, bg).asGlyph(fg, bg))
          List(NaturalSize.Col.atLeft$(glyphs))

        case EntityRef(id) =>
          import context.font
          import org.sufrin.glyph.{Text => TextChunk}
          def glyphOf(text: String): Glyph = toGlyph(TextChunk(text, font, context.fg, DefaultBrushes.nothing))
          val glyph = id match {
            case "amp" => glyphOf("&")
            case "ls" => glyphOf("<")
            case "gt" => glyphOf(">")
            case _ =>
              context.glyphMap.get(id) match {
                case None =>
                  warn(s"Unknown named glyph <copy id=$id ...>...")
                  glyphOf(s"&$id;")
                case Some(glyph) =>
                  if (inPara) context.atBaseLine(glyph) else glyph
              }
          }
          List(glyph)

        // TODO: should use the styled glyph
        case <button>{child@_*}</button> =>
          val context$ = attributes.declareAttributes(context)
          val act = localAttributes.Reaction("action", { _ => })(context)
          val label = localAttributes.String("label", "")
          val fg=context$.fg
          val bg=context$.bg
          val down       = attributes.Brush("down", ColourButton.down)
          val hover      = attributes.Brush("hover", ColourButton.hover)
          val background = attributes.Bool("background", false)
          lazy val glyphs = NaturalSize.Row(bg=bg).centered$(child.flatMap { node => translate(within$)(node)(attributes)(context$) })
          if (label.isEmpty)
            List(framed(ReactiveGlyphs.ColourButton(glyphs, down, hover, background)(act)))
          else
            List(framed(ReactiveGlyphs.ColourButton(label, fg, down, hover, bg, background)(act)))

        case <s></s> =>
          List(FixedSize.Space(1f, context.parSkip, 0f))

        case <font-cache></font-cache> =>
          import org.sufrin.glyph.{Text => TextChunk}
          val context$ = attributes.declareAttributes(context)
          val fg = context$.fg
          val bg = DefaultBrushes.nothing
          FontFamily.fonts.map{ fontId => toGlyph(TextChunk(fontId, context$.font, fg, bg))}

        case <p>{child@_*}</p> =>
          val context$ = attributes.declareAttributes(context)
          val skip = context$.parSkip
          val thePara = framed(GlyphML.glyphsToPara(child.flatMap {  node => translate(within$)(node)(attributes)(context$) })(context$))
          if (skip == 0.0f) List(thePara) else List(thePara, FixedSize.Space(1f, context.parSkip, 0f))

        case <verb>{child@_*}</verb> =>
          import org.sufrin.glyph.{Text => TextChunk}
          val context$ = attributes.declareAttributes(context)
          val background = attributes.Brush("bg", DefaultBrushes.nothing)
          val lines      = child.toString.split('\n').toSeq
          val lines$     = stripIndentation(lines)
          val texts      = lines$.map{ line => TextChunk(line, context$.font, context$.fg, context$.bg).asGlyph()}
          List(framed(NaturalSize.Col(bg=background).atLeft$(texts)))

        case <body>{child@_*}</body> =>
          val context$ = attributes.declareAttributes(context)
          child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <div>{child@_*}</div> =>
          val context$ = attributes.declareAttributes(context)
          import org.sufrin.glyph.{Text => TextChunk}
          localAttributes.get("id") match {
            case None =>
              child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
            case Some(id) =>
              context.attrMap.get(id) match {
                case None =>
                  org.sufrin.logging.Default.warn(s"""<div ${elem.attributes}>... has no attributes defined by $id""")
                  child.flatMap { node => translate(within$)(node)(attributes)(context$) }
                case Some(attrs) =>
                  val attributes$ = attrs++(localAttributes.removed("id"))
                  child.flatMap {  node => translate(within$)(node)(attributes$)(attributes$.declareAttributes(context)) }
              }
          }

        case <indent>{child@_*}</indent> =>
             val context$ = attributes.declareAttributes(context).indentEms(attributes.Int("l", 0), attributes.Int("r", 0))
             child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <i>{child@_*}</i> =>
          val context$ = attributes.declareAttributes(context).italicStyle
          child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <b>{child@_*}</b> =>
          val context$   = attributes.declareAttributes(context).boldStyle
          child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <bi>{child@_*}</bi> =>
          val context$   = attributes.declareAttributes(context).boldItalicStyle
          child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <n>{child@_*}</n> =>
          val context$   = attributes.declareAttributes(context).normalStyle
          child.flatMap {  node => translate(within$)(node)(attributes)(context$) }

        case <row>{child@_*}</row> =>
          val context$ = attributes.declareAttributes(context)
          val builder = NaturalSize.Row(fg=context.fg, bg=context.bg)
          val glyphs = child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
          val glyph = attributes.String("alignment", "center") match {
            case "center" => builder.centered$(glyphs)
            case "top"    => builder.atTop$(glyphs)
            case "bottom" => builder.atBottom$(glyphs)
            case other =>
              warn(s"alignment(=$other) should be center/top/bottom [using 'top']\nat <${elem.label}${elem.attributes}>")
              builder.atTop$(glyphs)
          }
          List(glyph)

        case <col>{child@_*}</col> =>
          val context$ = attributes.declareAttributes(context)
          val builder = NaturalSize.Col(fg=context.fg, bg=context.bg)
          val glyphs = child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
          val glyph = attributes.String("alignment", "center") match {
            case "center" => builder.centered$(glyphs)
            case "left"    => builder.atLeft$(glyphs)
            case "right" => builder.atRight$(glyphs)
            case other =>
              warn(s"alignment(=$other) should be center/left/right [using 'center']\nat <${elem.label}${elem.attributes}>")
              builder.centered$(glyphs)
          }
          List(glyph)

        case <rows>{child@_*}</rows> =>
          val context$ = attributes.declareAttributes(context)
          val width = attributes.Int("colWidth", 1)
          val glyphs = child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
          List(GlyphML.RowsFromGlyphs(width)(glyphs).toGlyph(context$))

        case <cols>{child@_*}</cols> =>
          val context$ = attributes.declareAttributes(context)
          val height = attributes.Int("rowHeight", 1)
          val glyphs = child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
          List(GlyphML.ColsFromGlyphs(height)(glyphs).toGlyph(context$))

        case <glyph>{child@_*}</glyph> =>
          val context$ = attributes.declareAttributes(context)
          val id = localAttributes.String("ref", "")
          id match {
            case "" =>
              val glyphs = child.flatMap {  node => translate(within$)(node)(attributes)(context$) }
              glyphs.map(context.atBaseLine(_))

            case _ =>
                  context.glyphMap.get (id) match {
                      case None =>
                      warn (s"Unknown named glyph <glyph ref=$id ...>...")
                      Seq.empty[Glyph]
                      case Some (glyph) =>
                      List (context.atBaseLine (glyph).framed() )
                  }
      }

        case elem: Elem =>
          val tag = elem.label
          context.generatorMap.get(tag) match {
            case None =>
              warn(s"Unknown tag <$tag ...")
              Seq.empty[Glyph]

            case Some(generator) =>
              List(generator(localAttributes, attributes.declareAttributes(context)))
          }
        case _ =>
          Seq.empty[Glyph]
      }
  }

  def hexToInt(s: String): Int = {
    s.toLowerCase.toList.map("0123456789abcdef".indexOf(_)).reduce (_ * 16 + _) // { (l,d) => (l * 16 + d)}
  }

  private def indentation(s: String): Int = {
    var n = 0
    while (n<s.length && s(n)==' ') n+=1
    n
  }

  private val indentation: Ordering[String] = new Ordering[String] {
    def compare(x: String, y: String): Int = indentation(x)-indentation(y)
  }

  def stripIndentation(lines: Seq[String]): Seq[String] = {
    val lines$ = lines.dropWhile(_.isBlank)
    val prefix = indentation(lines$.min(indentation))
    if (lines$.last.isBlank)
       lines$.init.map(_.substring(prefix))
    else
       lines$.map(_.substring(prefix))
  }
}
