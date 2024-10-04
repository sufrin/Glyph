package org.sufrin.glyph

import scala.collection.immutable.HashMap

object XML {
  def apply(node: xml.Node)(implicit sheet: StyleSheet): Glyph = {
    val local = GlyphML.Context(style=sheet)
    val glyphs: Seq[Glyph] = GlyphXML.translate(node)(Map.empty)(local)
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

    def Units(key: String, alt: Float)(context: GlyphML.Context): Float = attributes.get(key) match {
      case Some (s"${s}em") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat*context.emWidth
      case Some (s"${s}EM") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat*context.emWidth
      case Some (s"${s}") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
      case Some (s"${s}px") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
      case Some (s"${s}PX") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
      case Some(other)    =>
        warn(s"$key(=$other) should specify a unit in em/px")(source)
        alt
      case None => alt
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
        case "nothing" => DefaultBrushes.nothing
        case _ => DefaultBrushes.black
      }
    }
  }

  val glyphMapping: HashMap[String, (AttributeMap, Context)=>Glyph] = HashMap[String, (AttributeMap, Context)=>Glyph](
    "square" -> { case (localAttributes, local) =>
      val w = localAttributes.Float("w", 10)
      val h = localAttributes.Float("h", 10)
      Glyphs.Rect(w,h,fg=local.fg, bg=local.bg) }
  )

  def COLUMN(node: xml.Node)(implicit local: GlyphML.Context): GlyphML.Element = {
    val glyphs = translate(node)(Map.empty)(local)
    GlyphML.Column$(glyphs.map(GlyphML.GlyphElement(_)))
  }

  def ROW(node: xml.Node)(implicit local: GlyphML.Context): GlyphML.Element = {
    val glyphs = translate(node)(Map.empty)(local)
    GlyphML.Row$(glyphs.map(GlyphML.GlyphElement(_)))
  }


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
  def translate(elem: Node)(inherited: AttributeMap)(context: GlyphML.Context): Seq[Glyph] = {
     val localAttributes =  elem.attributes.asAttrMap
     val attributes = inherited ++ localAttributes
     def String(key: String, alt: String): String = attributes.String(key, alt)
     def Int(key: String, alt: Int): Int = attributes.Int(key, alt)
     def Float(key: String, alt: Float): Float = attributes.Float(key, alt)
     def Align(key: String, default: Alignment): Alignment = attributes.Align(key, default)
     def Brush(key: String, alt: Brush): Brush = attributes.Brush(key, alt)
    def Units(key: String, alt: Float): Float = attributes.Units(key, alt)(context)
    @inline def toGlyph(word: org.sufrin.glyph.Text): Glyph = word.atBaseline()

    /**
       * Context derived from this context by assigning
       * the universally-applicable attributes to it.
       */
      def universal(context: Context): Context = context
        .copy(parAlign=Align("align", context.parAlign),
              fontFamily=FontFamily(String("fontFamily", context.fontFamily.name)),
              fontSize=Float("fontSize", context.fontSize),
              paragraphWidth=Units("width", context.paragraphWidth),
              fg=Brush("fg", DefaultBrushes.black),
              bg=Brush("bg", DefaultBrushes.nothing),
              leftMargin = Float("leftMargin",   context.leftMargin),
              rightMargin = Float("rightMargin", context.leftMargin),
              )
        .indentEms(Int("l", 0), Int("r", 0))
        .fontScale(Float("fontScale", 1.0f))
        .parSkip(Units("parSkip", 0f))

      def framed(glyph: Glyph): Glyph =
        {
          val brush = Brush("framed", DefaultBrushes.nothing)
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
          List(toGlyph(TextChunk(text, font, fg, bg)))

        case <s></s> =>
          List(FixedSize.Space(1f, context.parSkip, 0f))

        case <font-cache></font-cache> =>
          import org.sufrin.glyph.{Text => TextChunk}
          val context$ = universal(context)
          val fg = context$.fg
          val bg = DefaultBrushes.nothing
          FontFamily.fonts.map{ fontId => toGlyph(TextChunk(fontId, context$.font, fg, bg))}

        case <p>{child@_*}</p> =>
          val context$ = universal(context)
          List(framed(GlyphML.glyphsToPara(child.flatMap {  node => translate(node)(attributes)(context$) })(context$)))

        case <verb>{child@_*}</verb> =>
          import org.sufrin.glyph.{Text => TextChunk}
          val context$ = universal(context)
          val background = Brush("bg", DefaultBrushes.nothing)
          val lines      = child.toString.split('\n').toSeq
          val lines$     = stripIndentation(lines)
          val texts      = lines$.map{ line => TextChunk(line, context$.font, context$.fg, context$.bg).asGlyph()}
          List(framed(NaturalSize.Col(bg=background).atLeft$(texts)))

        case <xml>{child@_*}</xml> =>
          val context$ = universal(context)
          child.flatMap {  node => translate(node)(attributes)(context$) }

        case <indent>{child@_*}</indent> =>
             val context$ = universal(context).indentEms(Int("l", 0), Int("r", 0))
             child.flatMap {  node => translate(node)(attributes)(context$) }

        case <i>{child@_*}</i> =>
          val context$ = universal(context).italicStyle
          child.flatMap {  node => translate(node)(attributes)(context$) }

        case <b>{child@_*}</b> =>
          val context$   = universal(context).boldStyle
          child.flatMap {  node => translate(node)(attributes)(context$) }

        case <bi>{child@_*}</bi> =>
          val context$   = universal(context).boldItalicStyle
          child.flatMap {  node => translate(node)(attributes)(context$) }

        case <n>{child@_*}</n> =>
          val context$   = universal(context).normalStyle
          child.flatMap {  node => translate(node)(attributes)(context$) }

        case <row>{child@_*}</row> =>
          val context$ = universal(context)
          val builder = NaturalSize.Row(fg=context.fg, bg=context.bg)
          val glyphs = child.flatMap {  node => translate(node)(attributes)(context$) }
          val glyph = String("alignment", "center") match {
            case "center" => builder.centered$(glyphs)
            case "top"    => builder.atTop$(glyphs)
            case "bottom" => builder.atBottom$(glyphs)
            case other =>
              warn(s"alignment(=$other) should be center/top/bottom [using 'top']\nat <${elem.label}${elem.attributes}>")
              builder.atTop$(glyphs)
          }
          List(glyph)

        case <col>{child@_*}</col> =>
          val context$ = universal(context)
          val builder = NaturalSize.Col(fg=context.fg, bg=context.bg)
          val glyphs = child.flatMap {  node => translate(node)(attributes)(context$) }
          val glyph = String("alignment", "center") match {
            case "center" => builder.centered$(glyphs)
            case "left"    => builder.atLeft$(glyphs)
            case "right" => builder.atRight$(glyphs)
            case other =>
              warn(s"alignment(=$other) should be center/left/right [using 'center']\nat <${elem.label}${elem.attributes}>")
              builder.centered$(glyphs)
          }
          List(glyph)

        case <rows>{child@_*}</rows> =>
          val context$ = universal(context)
          val width = Int("colWidth", 1)
          val glyphs = child.flatMap {  node => translate(node)(attributes)(context$) }
          List(GlyphML.RowsFromGlyphs(width)(glyphs).toGlyph(context$))

        case <cols>{child@_*}</cols> =>
          val context$ = universal(context)
          val height = Int("rowHeight", 1)
          val glyphs = child.flatMap {  node => translate(node)(attributes)(context$) }
          List(GlyphML.ColsFromGlyphs(height)(glyphs).toGlyph(context$))

        case elem: Elem =>
          val tag = elem.label
          glyphMapping.get(tag) match {
            case None =>
              warn(s"Unknown tag <$tag ...")
              Seq.empty[Glyph]

            case Some(generator) =>
              List(generator(localAttributes, universal(context)))
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
