package org.sufrin.glyph

import Brush.SQUARE
import Glyphs.BUTT
import GlyphTypes.{Font, FontStyle, Scalar}
import GlyphXML.{hexToInt, source, warn, AttributeMap}
import ReactiveGlyphs.{ColourButton, Reaction}
import Styles._
import Styles.Decoration.{Framed, Unframed}

import scala.collection.mutable
import scala.xml._

object TestHex {
  def main(args: Array[String]): Unit = {
    for { arg <- args } println(s"$arg => ${GXML.hexToInt(arg)}")
  }
}
object TestGXML extends Application {
  val translation = new GXML {}

  implicit def XML2Glyph(elem: Elem): Glyph = {
    val within = List("TOP")
    NaturalSize.Col().atLeft$(translation.translate(List(""))(within)(elem)(Map.empty)(new Sheet()))
  }

  val test0w = "50em"
  val test0 =
    <body fontFamily="Courier" fontSize="16" width={test0w} align="justify" parSkip="0.4ex" framed="darkGrey" padX="3em" padY="3ex" background="yellow" textBackground="yellow">
      <p>The rain in Spain falls mainly in the <i>hispanic</i> plain. This is justified in {s"$test0w"}</p>
      <p>The overall parSkip is 3.5ex</p>
      <p>
        This is some chatter to force the
        first part of a <b>paragraph</b> to the right edge.
        You are about to see an indent within the paragraph.
      </p>
      <p leftMargin="6em">
        This is a paragraph indented by 6em. It should still extend to the
        rightmost margin and be justified  there.
      </p>
      <p>This is just a short paragraph.</p>
      <p rightMargin="6em">
        This is a paragraph narrowed by 6em. It should still
        be justified at its right margin.
      </p>
      <p>This is just another short paragraph.</p>
      <p leftMargin="6em" rightMargin="6em">
        This is a paragraph both indented and narrowed by 6em.
        Its text should be justified, but as a whole it should appear centred
        in the space occupied by the overall paragraph
      </p>
      <p>
        This is some chatter to force the
        last part of the paragraph to the right edge.
      </p>
      <row width="50em" align="center">
        <fill/>
        <i>Columns: </i><fill stretch="0.3"/>
        <table cols="3" padY="2em" padX="2em">
          1 2 3 4 <col>5a 5b 5c</col> 6 7 8 9 10 11 12
        </table>
        <fill stretch="3"/>
        <i>Rows:</i><fill stretch="0.3"/>
        <table rows="3" padY="2em"  padX="2em">
          1 2 3 4 <col>5a 5b 5c</col> 6 7 8 9 10 11 12
        </table>
        <fill/>
      </row>
    </body>

  val title = "TestGXML"
  val GUI: Glyph = test0
}

object GXML {

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

    def Reaction(key: String, alt: Reaction)(context: GXML): Reaction = attributes.get(key) match {
      case Some (reactionName) =>
        context.reactionMap.getOrElse(reactionName, { _ => })
      case None =>
        alt
    }

    def Units(key: String, alt: Float)(context: Sheet): Float = attributes.get(key) match {
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
    def namedColour(name: String): Brush = {
      def common(name: String): Brush = name.toLowerCase match {
        case "red" => org.sufrin.glyph.Brush(s"red")(color = 0XFFFF0000)
        case "blue" => org.sufrin.glyph.Brush(s"blue")(color = 0XFF0000FF)
        case "green" => org.sufrin.glyph.Brush(s"green")(color = 0XFF00FF00)
        case "lightgrey" => org.sufrin.glyph.Brush(s"lightgrey")(color = 0XFFBBBBBB)
        case "darkgrey" => org.sufrin.glyph.Brush(s"darkgrey")(color = 0XFF777777)
        case "yellow" => org.sufrin.glyph.Brush(s"yellow")(color = 0XFFFFDD00)
        case "nothing" => org.sufrin.glyph.Brush(s"nothing")(color = 0XFF00FF00)
        case s"0X$hex" if hex.matches("[0-9A-F][0-9A-F][0-9A-F][0-9A-F]") =>
          org.sufrin.glyph.Brush(s"0X$hex")(color = hexToInt(hex))
        case _ =>
          warn(s"$name is not the name of a colour")
          org.sufrin.glyph.Brush(s"red($name)")(color = 0X99FF0000)
      }
      name match {
        case s"$name/$stroke/$cap" if stroke.matches("[0-9]+([.][0-9]+)?") =>
          val capShape = cap.toUpperCase match {
            case "ROUND" => ROUND
            case "SQUARE" => SQUARE
            case "BUTT" | "FLAT" => BUTT
            case _ => BUTT
          }
          common(name)(width=stroke.toFloat, cap=capShape)
        case s"$name/$stroke" if stroke.matches("[0-9]+([.][0-9]+)?") =>
          common(name)(width=stroke.toFloat, cap=SQUARE)
        case _ =>
          common(name)
      }
    }

    val brushCache = collection.mutable.HashMap[String, Brush]()

    def Brush(key: String, alt: Brush): Brush = attributes.get(key) match {
      case None       => alt
      case Some(name) =>
        brushCache.get(name) match {
          case Some(brush) =>
            brush
          case None =>
            val brush = namedColour(name)
            brushCache(name)=brush
            brush
        }
    }

    /**
     * Context derived from `context` by declaring
     * the universally-applicable attributes.
     *
     * This is stinkingly inelegant.
     */
    def declareAttributes(context: Sheet): Sheet = {
      val kore = context.core
      val context$: Sheet = {
        new Sheet(
          kore
            .copy(
              parAlign = Align("align", kore.parAlign),
              textFontFamily = FontFamily(String("fontFamily", kore.textFontFamily.name)),
              textFontSize = Float("fontSize", kore.textFontSize),
              // TODO: button and label font attributes
              fontScale = Float("fontScale", 1.0f),
              textBackgroundBrush = Brush("textBackground", kore.textBackgroundBrush),
              textForegroundBrush = Brush("textForeground", kore.textForegroundBrush)
            )
        )
      }
      val padX = Units("padX", kore.padX)(context$)
      val padY = Units("padY", kore.padY)(context$)
      val parWidth = Units("width", kore.parWidth)(context$)
      val parSkip = Units("parSkip", kore.parSkip)(context$)
      val leftMargin = Units("leftMargin", kore.leftMargin)(context$)
      val rightMargin = Units("rightMargin", kore.rightMargin)(context$)
        new Sheet(context$.core.copy(padX=padX, padY=padY, parWidth=parWidth, parSkip=parSkip, leftMargin=leftMargin, rightMargin = rightMargin))
    }
  }


  /**  Derive a glyph from `glyph`. The result has the same dimensions, appearance and behaviour, but
   *   will appear to have a baseline of `exHeight`, and will align with any `atBaseline`d texts set in
   *   the row in which it will be composed.
   *
   *   TODO: (Sept 2024) the entire business of baselines needs rethinking. Right now glyphs have
   *         baselines (0 except for atBaselined text glyphs) that allow them to be composed in
   *         /all/ horizontal rows so that atBaselined texts align along their baselines. It might
   *         be better to introduce the idea of a "galley" row: designed only for glyphs with
   *         baselines, to let nonGalleys ignore constituent baselines, and to have galleys
   *         compute their own baselines from their constituents, and align their constituents
   *         accordingly.
   */
  def simulateBaseline(glyph: Glyph, exHeight: Scalar): Glyph = new Glyph { thisGlyph =>
    locally { glyph.parent=thisGlyph }

    def draw(surface: Surface): Unit = {
      surface.withOrigin(0, -exHeight) {
        glyph.draw(surface)
      }
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
      glyph.reactiveContaining(p-(0, exHeight))

    override def glyphContaining(p: Vec): Option[Hit] =
      glyph.glyphContaining(p-(0, exHeight))

    override def baseLine: Scalar = exHeight

    def diagonal: Vec = Vec(glyph.w, glyph.h)

    def copy(fg: Brush, bg: Brush): Glyph = simulateBaseline(glyph, exHeight)

    val fg: Brush = glyph.fg
    val bg: Brush = glyph.bg
  }

  /**  */
  def glyphsToPara(glyphs: Seq[Glyph])(local: Sheet): Glyph = {
    val emWidth   = local.emWidth
    val interWord = FixedSize.Space(w=emWidth / 1.5f, h=0f, stretch = 2f)
    val glyphs$   = local.core.parIndent() ++ glyphs

    // The overall width is determined by the context
    // If the bounding box is unspecified, then use the column width
    val galley =
      styled.text.glyphParagraph(
        overallWidth = local.core.parWidth,
        align        = local.core.parAlign,
        leftMargin   = local.core.leftMargin,
        rightMargin  = local.core.rightMargin,
        interWord,
        glyphs$
      )

    val column = NaturalSize.Col(bg = local.core.textBackgroundBrush).atLeft$(galley.toSeq)
    if (local.core.leftMargin > 0f)
      NaturalSize.Row(bg = local.core.textBackgroundBrush)
        .centered(FixedSize.Space(w=local.core.leftMargin, h=0f, stretch=0f),
          column,
          FixedSize.Space(w=local.core.rightMargin, h=0f, stretch=0f))
    else
      column
  }

  def hexToInt(s: String): Int = {
    s.toLowerCase.toList.map("0123456789abcdef".indexOf(_)).reduce (_ * 16 + _) // { (l,d) => (l * 16 + d)}
  }

}

class GXML {
  import GXML._
  val glyphMap:     mutable.Map[String, Glyph] = mutable.LinkedHashMap[String, Glyph]()
  val generatorMap: mutable.Map[String, (AttributeMap, Sheet)=>Glyph] = mutable.LinkedHashMap[String, (AttributeMap, Sheet)=>Glyph]()
  val attrMap:      mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val sheetMap:     mutable.Map[String, Sheet] = mutable.LinkedHashMap[String, Sheet]()
  val reactionMap:  mutable.Map[String, Reaction] = mutable.LinkedHashMap[String, Reaction]()

  private val stylingTags: Seq[String] = List("b", "em", "i", "indent", "bi", "n")

  def translate(sources: List[String])(within: List[String])(elem: Node)(inherited: AttributeMap)(context: Sheet): Seq[Glyph] = {
    def warning(message: => String): Unit = {
      org.sufrin.logging.Default.warn(s"$message")
      org.sufrin.logging.Default.warn(s"Source: ${sources.head}")
      org.sufrin.logging.Default.warn(s"Nested: ${within.mkString(":")}")
    }
    def attrsFor(attrId: String): AttributeMap = {
      elem.attributes.asAttrMap.get(attrId) match {
        case None       => Map.empty
        case Some(attr) =>
          attrMap.get(attr) match {
            case None =>
              warning(s"no defaults for $attrId=\"$attr\"")
              Map.empty
            case Some(attrs) =>
              attrs
          }
      }
    }

    val defaultAttributes: AttributeMap = attrMap.getOrElse(s"#${elem.label}", Map.empty) ++ attrsFor("class") ++ attrsFor("id")
    val localAttributes = defaultAttributes ++ (elem.attributes.asAttrMap)
    val attributes = inherited ++ localAttributes
    val within$ = elem.label :: within
    val sources$ = (attributes.getOrElse("source", "<unknown>")) :: sources

    def stylingTag(tag: String): Boolean = stylingTags.contains(tag)
    val inPara: Boolean = within.dropWhile(stylingTag).head=="p"


    @inline def toGlyph(word: org.sufrin.glyph.Text, fg: Brush): Glyph =
      if (inPara) word.atBaseline(fg=fg) else word.asGlyph(fg=fg)

    def framed(glyph: Glyph): Glyph = {
      val brush = attributes.Brush("framed", DefaultBrushes.nothing)
      if (brush ==  DefaultBrushes.nothing)
        glyph
      else
        glyph.framed(fg=brush, bg=attributes.Brush("bg", glyph.bg))
    }

    def isBlank(elem: Node): Boolean = elem match {
      case Text(data) => data.isBlank
      case _ => false
    }


    elem match {
      case Text(buffer) =>
        import context.core.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        buffer.toString.split("[\n\t ]+").toSeq.filterNot(_.isBlank).map{ word => toGlyph(TextChunk(word, textFont, fg, bg), fg) }

      case PCData(text) =>
        import context.core.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        val glyphs = text.split('\n').toSeq.map(TextChunk(_, textFont, fg, bg).asGlyph(fg, bg))
        List(NaturalSize.Col.atLeft$(glyphs))

      case EntityRef(id) =>
        import context.core.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        def glyphOf(text: String): Glyph = toGlyph(TextChunk(text, textFont, fg=fg, bg=bg), fg)
        val glyph = id match {
          case "amp" => glyphOf("&")
          case "ls" => glyphOf("<")
          case "gt" => glyphOf(">")
          case _ =>
            glyphMap.get(id) match {
              case None =>
                warning(s"Unknown named glyph <copy id=$id ...>...")
                glyphOf(s"&$id;")
              case Some(glyph) =>
                // We are laying out a paragraph, most of which will really be at a baseline
                // TODO: Think again about baseline alignments of texts and all other glyphs
                if (inPara) simulateBaseline(glyph, context.exHeight) else glyph
            }
        }
        List(glyph)

      // TODO: NEEDS COMPLETE REVISION (styled glyph?)
      case <button>{child@_*}</button> =>
        val context$   = attributes.declareAttributes(context)
        val act        = localAttributes.Reaction("action", { _ => })(this)
        val label      = localAttributes.String("label", "")
        val fg         = localAttributes.Brush("fg", context$.buttonStyle.up.fg)
        val bg         = localAttributes.Brush("bg", context$.buttonStyle.up.bg)
        val up         = localAttributes.Brush("up", fg)
        val down       = attributes.Brush("down", ColourButton.down)
        val hover      = attributes.Brush("hover", ColourButton.hover)
        val background = attributes.Bool("background", false)
        lazy val glyphs = NaturalSize.Row(bg=bg).centered$(child.flatMap { node => translate(sources$)(within$)(node)(attributes)(context$) })
        if (label.isEmpty)
          List(framed(ReactiveGlyphs.ColourButton(glyphs, down, hover, background)(act)))
        else
          List(framed(ReactiveGlyphs.ColourButton(label, up, down, hover, bg, background)(act)))

      case <s></s> =>
        List(FixedSize.Space(1f, context.core.parSkip, 0f))

      case <fill></fill> =>
        val stretch = localAttributes.Float("stretch", 1f)
        List(FixedSize.Space(1f, 1f, stretch))

      case <p>{child@_*}</p> =>
        val context$ = attributes.declareAttributes(context)
        val skip = context$.core.parSkip
        val thePara = framed(glyphsToPara(child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) })(context$))
        if (skip == 0.0f)
          List(thePara)
        else
          List(thePara above FixedSize.Space(1f, context.core.parSkip, 0f))

      case <verb>{child@_*}</verb> =>
        import org.sufrin.glyph.{Text => TextChunk}
        val context$ = attributes.declareAttributes(context)
        val local = context$.core
        val font = local.textFont
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val background = attributes.Brush("bg", DefaultBrushes.nothing)
        val lines      = child.toString.split('\n').toSeq
        val lines$     = stripIndentation(lines)
        val texts      = lines$.map{ line => TextChunk(line, font, fg, bg).asGlyph()}
        List(framed(NaturalSize.Col(bg=background).atLeft$(texts)))

      case <body>{child@_*}</body> =>
        val context$ = attributes.declareAttributes(context)
        val background = attributes.Brush("background", DefaultBrushes.nothing)
        //TODO: maybe these are strictly local to body
        val padX = context$.core.padX
        val padY = context$.core.padY
        //context.xmlSource=attributes.String("source", "<unknown>")
        val glyphs = child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(framed(NaturalSize.Col(bg=background).atLeft$(glyphs)).enlargedBy(padX, padY, bg=background))

      case <div>{child@_*}</div> =>
        val context$ = attributes.declareAttributes(context)
        localAttributes.get("id") match {
          case None =>
            child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
          case Some(id) =>
            attrMap.get(id) match {
              case None =>
                warning(s"""<div ${elem.attributes}>... has no attributes defined by $id""")
                child.flatMap { node => translate(sources$)(within$)(node)(attributes)(context$) }
              case Some(attrs) =>
                val attributes$ = attrs++(localAttributes.removed("id"))
                child.flatMap {  node => translate(sources$)(within$)(node)(attributes$)(attributes$.declareAttributes(context)) }
            }
        }

      case <indent>{child@_*}</indent> =>
        val context$ = attributes.declareAttributes(context).parNarrow(attributes.Units("l", 0)(context), attributes.Units("r", 0)(context))
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <i>{child@_*}</i> =>
        val context$ = attributes.declareAttributes(context).italicStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <b>{child@_*}</b> =>
        val context$   = attributes.declareAttributes(context).boldStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <bi>{child@_*}</bi> =>
        val context$   = attributes.declareAttributes(context).boldItalicStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <n>{child@_*}</n> =>
        val context$   = attributes.declareAttributes(context).normalStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <row>{child@_*}</row> if (localAttributes.get("width").isDefined) =>
        val context$ = attributes.declareAttributes(context)
        val width = localAttributes.Units("width", 0f)(context)
        val fg=context.core.textForegroundBrush
        val bg=context.core.textBackgroundBrush
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val builder  = FixedSize.Row.apply(width, fg=fg, bg=bg)

        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "top"    => builder.atTop$(glyphs)
          case "bottom" => builder.atBottom$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/top/bottom [using 'top']\nat <${elem.label}${elem.attributes}>")
            builder.atTop$(glyphs)
        }
        List(glyph)

      case <row>{child@_*}</row> =>
        val context$ = attributes.declareAttributes(context)
        val builder = NaturalSize.Row(fg=context.core.textForegroundBrush, bg=context.core.textBackgroundBrush)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "top"    => builder.atTop$(glyphs)
          case "bottom" => builder.atBottom$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/top/bottom [using 'top']\nat <${elem.label}${elem.attributes}>")
            builder.atTop$(glyphs)
        }
        List(glyph)

      case <col>{child@_*}</col> =>
        val context$ = attributes.declareAttributes(context)
        val builder = NaturalSize.Col(fg=context.core.textForegroundBrush, bg=context.core.textBackgroundBrush)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "left"    => builder.atLeft$(glyphs)
          case "right" => builder.atRight$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/left/right [using 'center']\nat <${elem.label}${elem.attributes}>")
            builder.centered$(glyphs)
        }
        List(glyph)

      case <table>{child@_*}</table> =>
        val context$ = attributes.declareAttributes(context)
        val local = context$.core
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val padX = local.padX
        val padY = local.padY
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(framed(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).table(height=height, width=width)(glyphs)))

      case <rows>{child@_*}</rows> =>
        val context$ = attributes.declareAttributes(context)
        val local = context$.core
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val padX = local.padX
        val padY = local.padY
        List(framed(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).rows(width=width)(glyphs)))

      case <cols>{child@_*}</cols> =>
        val context$ = attributes.declareAttributes(context)
        val local = context$.core
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val padX = local.padX
        val padY = local.padY
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(framed(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).cols(height=height)(glyphs)))


      case <grid>{child@_*}</grid> =>
        val context$ = attributes.declareAttributes(context)
        val local = context$.core
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val padX = local.padX
        val padY = local.padY
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(framed(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).grid(width=width, height=height)(glyphs)))

      case <glyph>{child@_*}</glyph> =>
        val context$ = attributes.declareAttributes(context)
        val id = localAttributes.String("ref", "")
        id match {
          case "" =>
            val glyphs = child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
            glyphs.map(simulateBaseline(_, context$.exHeight))

          case _ =>
            glyphMap.get (id) match {
              case None =>
                warning (s"Unknown named glyph <glyph ref=$id ...>...")
                Seq.empty[Glyph]
              case Some (glyph) =>
                List (simulateBaseline(glyph, context.exHeight).framed() )
            }
        }

      case elem: Elem =>
        val tag = elem.label
        generatorMap.get(tag) match {
          case None =>
            warning(s"Unknown tag <$tag ...")
            Seq.empty[Glyph]

          case Some(generator) =>
            List(generator(localAttributes, attributes.declareAttributes(context)))
        }
      case _ =>
        Seq.empty[Glyph]
    }
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

case class CoreSheet
( fontScale: Scalar = 1.0f,
  textFontFamily: FontFamily  = FontFamily(),
  textFontStyle: FontStyle = FontStyle.NORMAL,
  textFontSize: Scalar  = 22f,
  labelFontFamily: FontFamily  = FontFamily(),
  labelFontStyle: FontStyle = FontStyle.NORMAL,
  labelFontSize: Scalar  = 22f,
  buttonFontFamily: FontFamily  = FontFamily(),
  buttonFontStyle: FontStyle = FontStyle.NORMAL,
  buttonFontSize: Scalar  = 22f,
  buttonBorderBrush: Brush = Brush("buttonBorder")(color=0XFF777777, width=5f),
  buttonBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF), // transparent
  buttonForegroundBrush: Brush = Brush("buttonForeground")(color=0xFF0000FF), // blue
  buttonHoverBrush: Brush = Brush("buttonHover")(color=0xFF00FF00),           // green
  buttonDownBrush: Brush = Brush("buttonDown")(color=0xFFFF0000),             // red
  toggleBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF), // transparent
  toggleOnBrush: Brush = Brush("toggleOn")(color=0xFFFF0000),                 // red
  toggleOffBrush: Brush = Brush("toggleOn")(color=0xFF0000FF),                // blue
  labelBackgroundBrush: Brush = Brush("buttonBackground")(color=0X00FFFFFF),  // transparent
  labelForegroundBrush: Brush = Brush("buttonForeground")(color=0xFF0000FF),  // blue
  textBackgroundBrush: Brush = Brush("textBackground")(color=0X00FFFFFF),     // transparent
  textForegroundBrush: Brush = Brush("textForeground")(color=0xFF0000FF),     // blue
  // Layout properties
  parHang:     Boolean   = false,
  parAlign:    Alignment = Left,
  parSkip:     Scalar    = 5f,
  parWidth:    Scalar    = 200f,
  leftMargin:  Scalar=0f,
  rightMargin: Scalar=0f,
  parIndent:   ()=>Seq[Glyph] = { ()=>Nil },
  padX: Scalar = 0f,
  padY: Scalar = 0f,
) {
  val toggleOn = new GlyphColours {
    val fg: Brush = toggleOnBrush;
    val bg: Brush = toggleBackgroundBrush
  }
  val toggleOff = new GlyphColours {
    val fg: Brush = toggleOffBrush;
    val bg: Brush = toggleBackgroundBrush
  }
  def labelFont: Font = labelFontFamily.makeFont(labelFontStyle, labelFontSize*fontScale)
  def textFont: Font = textFontFamily.makeFont(textFontStyle, textFontSize*fontScale)
  def buttonFont: Font = buttonFontFamily.makeFont(buttonFontStyle, buttonFontSize*fontScale)
  def buttonBorderWidth: Scalar = buttonBorderBrush.strokeWidth
  def parNarrow(left: Scalar, right: Scalar): CoreSheet = {
    val lm = leftMargin
    val rm = rightMargin
    org.sufrin.logging.Default.info(s"parNarrow($left,$right) => ${(lm + left, rm+right)}")
    copy(leftMargin = lm + left, rightMargin = rm + right)
  }
}

/** A stylesheet is derived from its core parameters */
class Sheet(val core: CoreSheet = CoreSheet()) {
  baseSheet =>

  import core._

  lazy val labelStyle: GlyphStyle = GlyphStyle(labelFont, labelForegroundBrush, labelBackgroundBrush)

  lazy val buttonStyle: ButtonStyle = ButtonStyle(
    up = GlyphStyle(font = buttonFont, fg = buttonForegroundBrush, bg = buttonBackgroundBrush),
    down = GlyphStyle(font = buttonFont, fg = buttonDownBrush, bg = buttonBackgroundBrush),
    hover = GlyphStyle(font = buttonFont, fg = buttonHoverBrush, bg = buttonBackgroundBrush),
    frame = Framed(fg = buttonBorderBrush, bg = buttonBackgroundBrush, radiusFactor = 0.5f),
    border = 6f,
    toggle = ToggleStyle(on = toggleOn, off = toggleOff),
    checkbox = CheckboxStyle(tick = "✔", cross = "✖", on = toggleOn, off = toggleOff)
  )

  lazy val unFramed: Sheet = new Sheet(core) {
    override lazy val buttonStyle: ButtonStyle = baseSheet.buttonStyle.copy(frame = Decoration.Unframed)
  }

  lazy val menuStyle: MenuStyle = MenuStyle(
    button = buttonStyle,
    nestedButton = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    reactive = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    inactive = Unframed,
    bg = DefaultBrushes.lightGrey,
    fg = DefaultBrushes.lightGrey,
  )

  lazy val emWidth: Scalar = textFont.measureTextWidth("m")
  lazy val exHeight: Scalar = textFont.measureText("X").getHeight



  //  lazy val notebookStyle: NotebookStyle = NotebookStyle(
  //    buttonStyle = baseSheet.buttonStyle,
  //    pageStyle = baseSheet.buttonStyle
  //  )

  private def styled(fontStyle: FontStyle): Sheet = new Sheet(core.copy(textFontStyle = fontStyle))
  def italicStyle: Sheet = styled(FontStyle.ITALIC)
  def boldStyle: Sheet = styled(FontStyle.BOLD)
  def boldItalicStyle: Sheet = styled(FontStyle.BOLD_ITALIC)
  def normalStyle: Sheet = styled(FontStyle.NORMAL)

  def parNarrow(left: Scalar, right: Scalar): Sheet = new Sheet(core.parNarrow(left, right))

}


