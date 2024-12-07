package org.sufrin.glyph

import Brush.SQUARE
import Glyphs.{BreakableGlyph, BUTT}
import GlyphTypes.Scalar
import ReactiveGlyphs.{ColourButton, Reaction}

import org.sufrin.logging.SourceLoggable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._

object GlyphXML extends SourceLoggable {
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

    def Reaction(key: String, alt: Reaction)(context: GlyphXML): Reaction = attributes.get(key) match {
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
        case "white" => org.sufrin.glyph.Brush(s"white")(color = 0XFFFFFFFF)
        case "grey1" => org.sufrin.glyph.Brush(s"grey1")(color = 0XFFBBBBBB)
        case "grey2" => org.sufrin.glyph.Brush(s"grey2")(color = 0XFFCDCDCD)
        case "grey3" => org.sufrin.glyph.Brush(s"grey3")(color = 0XFFC5C5C5)
        case "grey4" => org.sufrin.glyph.Brush(s"grey4")(color = 0XFFC2C2C2)
        case "lightgrey" => org.sufrin.glyph.Brush(s"lightgrey")(color = 0XFFBBBBBB)
        case "darkgrey" => org.sufrin.glyph.Brush(s"darkgrey")(color = 0XFF777777)
        case "yellow" => org.sufrin.glyph.Brush(s"yellow")(color = 0XFFFFDD00)
        case "nothing" => org.sufrin.glyph.Brush(s"nothing")(color = 0X00000000)
        case "" => org.sufrin.glyph.Brush(s"nothing")(color = 0X00000000)
        case s"0x${hex}" if hex.matches("([0-9a-f])+") =>
          org.sufrin.glyph.Brush(s"0X$hex")(color = hexToInt(hex))
        case name =>
          org.sufrin.logging.Default.warn(s"$name is not the name of a colour")
          org.sufrin.glyph.Brush(s"red($name)")(color = 0XFFFF0000)
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
     * the universally-applicable attributes in
     * for the `Node`.
     */
    def declareAttributes(context: Sheet): Sheet = {
      val fontDetail: Sheet =
        context
          .copy(
            fontScale       = Float("fontScale",              1.0f),

            textFontFamily  = FontFamily(String("fontFamily", String("textFontFamily", context.textFontFamily.name))),
            textFontSize    = Float("fontSize", Float("textFontSize", context.textFontSize)),

            labelFontFamily = FontFamily(String("fontFamily", String("labelFontFamily", context.labelFontFamily.name))),
            labelFontSize   = Float("fontSize", Float("labelFontSize", context.labelFontSize)),

            buttonFontFamily = FontFamily(String("fontFamily", String("buttonFontFamily", context.buttonFontFamily.name))),
            buttonFontSize   = Float("fontSize", Float("buttonFontSize",                  context.buttonFontSize)),
          )

      // Units are computed relative to the font details, which may have been redeclared
      fontDetail.copy(
        padX                = Units("padX",           context.padX)         (fontDetail),
        padY                = Units("padY",           context.padY)         (fontDetail),
        parWidth            = Units("width",          context.parWidth)     (fontDetail),
        parSkip             = Units("parSkip",        context.parSkip)      (fontDetail),
        leftMargin          = Units("leftMargin",     context.leftMargin)   (fontDetail),
        rightMargin         = Units("rightMargin",    context.rightMargin)  (fontDetail),
        parAlign            = Align("align",          context.parAlign),
        textBackgroundBrush = Brush("textBackground", context.textBackgroundBrush),
        textForegroundBrush = Brush("textForeground", context.textForegroundBrush)
      )
    }
  }


  /**  Derive a glyph from `glyph`. The result has the same dimensions, appearance and behaviour, but
   *   will appear to have a baseline of `baseLine$`.
   *
   *   TODO: (Sept 2024) the entire business of baselines needs rethinking. Right now glyphs have
   *         baselines (0 except for atBaselined text glyphs) that allow them to be composed in
   *         /all/ horizontal rows so that atBaselined texts align along their baselines. It might
   *         be better to introduce the idea of a "galley" row: designed only for glyphs with
   *         baselines, to let nonGalleys ignore constituent baselines, and to have galleys
   *         compute their own baselines from their constituents, and align their constituents
   *         accordingly.
   */
  def withBaseline(glyph: Glyph, baseLine$: Scalar): Glyph = new Glyph { thisGlyph =>

    locally { glyph.parent=thisGlyph }

    def draw(surface: Surface): Unit = surface.withOrigin(0, -baseLine) {
        glyph.draw(surface)
      }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] = glyph.reactiveContaining(p)

    override def glyphContaining(p: Vec): Option[Hit] = glyph.glyphContaining(p)

    override def baseLine: Scalar = baseLine$

    def diagonal: Vec = Vec(glyph.w, glyph.h)

    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = {
      org.sufrin.logging.Default.info(s"copying $glyph.atBaseline($baseLine)")
      withBaseline(glyph.copy(fg, bg), baseLine$)
    }

    val fg: Brush = glyph.fg
    val bg: Brush = glyph.bg
  }


    /**
   *  Build a paragraph-formatted glyph formatted according to `sheet` from
   *  the `glyphs` sequence.
   */
  def glyphsToParagraph(glyphs: Seq[Glyph])(sheet: Sheet): Glyph = {
    val emWidth   = sheet.emWidth
    val interWord = FixedSize.Space(w=emWidth / 1.5f, h=0f, stretch = 2f)
    val glyphs$   = sheet.parIndent() ++ glyphs

    // The overall width is determined by the context
    // If the bounding box is unspecified, then use the column width
    val galley =
      formatParagraph(
        overallWidth = sheet.parWidth,
        align        = sheet.parAlign,
        leftMargin   = sheet.leftMargin,
        rightMargin  = sheet.rightMargin,
        interWord,
        glyphs$
      )

    val column = NaturalSize.Col(bg = sheet.textBackgroundBrush).atLeft$(galley.toSeq)

    if (sheet.leftMargin > 0f)
      NaturalSize.Row(bg = sheet.textBackgroundBrush)
        .centered(FixedSize.Space(w=sheet.leftMargin, h=0f, stretch=0f),
          column,
          FixedSize.Space(w=sheet.rightMargin, h=0f, stretch=0f))
    else
      column
  }

  /**
   * Build a sequence of galleys representing the lines of a paragraph
   * formed from `glyphs`.
   */
  def formatParagraph(overallWidth:  Scalar,
                      align:         Alignment,
                      leftMargin:    Scalar,
                      rightMargin:   Scalar,
                      interWord:     Glyph,
                      glyphs:        Seq[Glyph]) = {
    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    val maxWidthfloor  = maxWidth.floor
    //println(s"[ov=$overallWidth,lm=$leftMargin,maxw=$maxWidthfloor]")
    val interWordWidth = interWord.w
    val words = new StreamIterator[Glyph](glyphs.iterator)

    @inline def setLine(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      line += align.leftFill()
      while (words.hasElement && (lineWidth + words.element.w + interWordWidth < maxWidthfloor)) {
        words.element match {
          case BreakableGlyph(_, glyphs) =>
            for { glyph <- glyphs } line += glyph
          case other =>
            line += other
        }
        line += interWord()
        lineWidth += words.element.w + interWordWidth
        words.nextElement()
      }

      // squeeze an extra chunk on by splitting a splittable?
      if (words.hasElement) words.element match {
        case breakable: BreakableGlyph =>
          val breakPoint: Int = breakable.maximal(maxWidthfloor-lineWidth-interWordWidth-breakable.hyphen.w)
          if (breakPoint!=0) {
            val glyphs = breakable.glyphs
            for { i <- 0 until breakPoint } {
              lineWidth += glyphs(i).w
              line += glyphs(i)
            }
            line += breakable.hyphen()
            line += interWord()
            lineWidth += breakable.hyphen.w
            words.buffer = Some(new BreakableGlyph(breakable.hyphen, glyphs.drop(breakPoint)))
          }
        case _ =>
      }

      // line is full, erase the interword or leftfill
      line.update(line.length - 1, align.rightFill())
      // if this is the very last line, words will be empty
      if (!words.hasElement) {
        line += align.lastFill()
      }
      (lineWidth, line.toSeq)
    }

    var setting = true
    while (setting && words.hasElement) {
      // Maybe we have an overlong word
      if (words.hasElement && words.element.w.ceil >= maxWidthfloor) {
        // Shrink it somehow
        words.element match {
          // cram in as much as possible, and omit the rest
          case breakableGlyph: BreakableGlyph if false  =>
            val glyphs = breakableGlyph.glyphs
            val breakPoint: Int = breakableGlyph.maximal(maxWidthfloor)
            galley += NaturalSize.Row.atTop$(glyphs.take(breakPoint)).framed(fg = DefaultBrushes.red(width=2))
          case other =>
            galley += other.scaled(maxWidthfloor/other.w).framed(fg = DefaultBrushes.nothing, bg=DefaultBrushes.lightGrey)
        }
        words.nextElement()
      } else {
        val (width, glyphs) = setLine()
        // the line had only its starting alignment glyph; nothing else to do
        if (glyphs.length == 1 && width == 0) {
          setting = false
        } else
          galley += FixedSize.Row(maxWidth).atTop$(glyphs)
      }
    }
    galley
  }


  /**
   *   Pre: `s` consists only of hexadecimal characters
   *   @return the integer represented in hexadecimal by the string `s`.
   */
  def hexToInt(s: String): Int = {
    s.toLowerCase.toList.map("0123456789abcdef".indexOf(_)).reduce (_ * 16 + _) // { (l,d) => (l * 16 + d)}
  }

}

/**
 * The semantic specification of XML that denotes a glyph.
 */
class GlyphXML {
  import GlyphXML._

  // Mappings from names (used within XML) to relevant glyphs or their aspects.
  private val glyphMap:     mutable.Map[String, ()=>Glyph] = mutable.LinkedHashMap[String, ()=>Glyph]()
  private val generatorMap: mutable.Map[String, (AttributeMap, Sheet)=>Glyph] = mutable.LinkedHashMap[String, (AttributeMap, Sheet)=>Glyph]()
  private val attrMap:      mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  private val sheetMap:     mutable.Map[String, Sheet] = mutable.LinkedHashMap[String, Sheet]()
  private val reactionMap:  mutable.Map[String, Reaction] = mutable.LinkedHashMap[String, Reaction]()
  private val entityMap:    mutable.Map[String, String] = mutable.LinkedHashMap[String, String]()

  /** Declare a named glyph */
  def update(id: String, glyph: => Glyph): Unit = glyphMap(id)={ ()=> glyph }
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = attrMap(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: (AttributeMap, Sheet)=>Glyph) = generatorMap(id) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = entityMap(id) = expansion


  private val stylingTags: Seq[String] = List("b", "em", "i", "bi", "n", "hyph", "glyph")
  def stylingTag(tag: String): Boolean = stylingTags.contains(tag)

  /**
   * Translate  `elem: Node` to the list of glyphs that it denotes.
   *
   * @param sources List of source locations of the nodes within which the `elem: Node` are nested.
   * @param within  List of source tags of the nodes within which the `elem: Node` are nested.
   * @param elem    The `Node` being translated.
   * @param inherited Map of the attributes inherited from the nodes within which `elem` is nested.
   * @param context   The style-sheet effective for the defs of `elem`.
   * @return The list of glyphs denoted by `elem`: sometimes a singleton list.
   *
   *         TODO: this should be transformed into a streaming translator. The problem is that adjacent nodes can (and generally do) give rise to
   *               distinct "word glyphs" within paragraphs. EG: "<i>italic</i>" gives rise to 3 distinct word glyphs, and
   *               "&entityref;" gives rise to (n+2) when the referenced entity gives rise to n.
   *               The words will (in  general) be set with spaces between them. There is no systematic workaround, though the
   *               notation <embed>prefix&entityref;suffix</embed> adjoins the prefix to the opening "word" of the referenced entity;
   *               and the suffix to its closing "word".
   */
  def translate(sources: List[String])(within: List[String])(elem: Node)(inherited: AttributeMap)(context: Sheet): Seq[Glyph] = {

    /** Output an element-specific warning to the log. */
    def warning(message: => String): Unit = {
      org.sufrin.logging.Default.warn(s"$message")
      org.sufrin.logging.Default.warn(s"Source: ${sources.reverse.mkString(" ")}")
      org.sufrin.logging.Default.warn(s"      : ${within.reverse.mkString("", "<", "")}")
    }

    /**
     * The (globally-declared) attributes corresponding to the
     * given attribute name.
     */
    def attrsFor(attrId: String): AttributeMap = {
      elem.attributes.asAttrMap.get(attrId) match {
        case None       => Map.empty
        case Some(attr) =>
          attrMap.get(attr) match {
            case None =>
              warning(s"no defaults for $attrId=\"$attr\"")
              Map.empty
            case Some(attrs) =>
              org.sufrin.logging.Default.info(s"attrsFor($attrId) $attr = $attrs")
              attrs
          }
      }
    }

    /**
     * The default attributes of an element with tag `label` are the catenation of the
     * globally-declared attributes for `#label`, then those of its declared "class",
     * then those of its specific "id".
     */
    val defaultAttributes: AttributeMap = attrMap.getOrElse(s"#${elem.label}", Map.empty) ++ attrsFor("class") ++ attrsFor("id")

    /**
     * The purely-local attributes of an element are the catenation of its default attributes and its actually-appearing
     * attributes.
     */
    val localAttributes = defaultAttributes ++ (elem.attributes.asAttrMap)

    /**
     * The effective attributes of an element are catenated from its inherited attributes
     * and its local attributes.
     */
    val attributes = inherited ++ localAttributes

    val within$ = elem.label :: within
    val sources$ = (attributes.getOrElse("source", "")) :: sources

    val inPara: Boolean = within.dropWhile(stylingTag).head=="p"

    @inline def toGlyph(word: org.sufrin.glyph.Text, fg: Brush): Glyph =
      if (inPara) word.atBaseline(fg=fg) else word.asGlyph(fg=fg)

    /**
     * Glyphs may be decorated by being framed and/or rotated.
     */
    def decorated(glyph: Glyph): Glyph = {
      val g0 = glyph
      def rotate(glyph: Glyph): Glyph = {
        val rotated = localAttributes.Int("rotated", 0)
        rotated % 4 match {
          case 0     => glyph
          case 1 | 3 => withBaseline(glyph.rotated(rotated), glyph.w)
          case 2     => withBaseline(glyph.rotated(rotated), glyph.h)
        }
      }

      def turn(glyph: Glyph): Glyph = {
        val degrees: Scalar = localAttributes.Int("turned", 0).toFloat
        val glyph$ = glyph.turned(degrees, false)
         glyph$//atBaseline(glyph$, glyph$.h)
      }

      def frame(glyph: Glyph): Glyph = {
        val brush = attributes.Brush("frame", DefaultBrushes.nothing)
        val framing = attributes.Bool("framed", brush.getAlpha != 0)
        if (framing)
          glyph.framed(fg = brush, bg = attributes.Brush("bg", glyph.bg))
        else
          glyph
      }

      rotate(frame(turn(glyph)))
    }


    def isBlank(elem: Node): Boolean = elem match {
      case Text(data) => data.isBlank
      case _ => false
    }

    def translateText(buffer: String): Seq[Glyph] = {
      import context.{textFont, textForegroundBrush => fg}
      import org.sufrin.glyph.{Text => TextChunk}
      val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
      buffer.toString.split("[\n\t ]+").toSeq.filterNot(_.isBlank).map {
        case s"$$$word" =>
          val id = if (word endsWith "$") word.substring(0, word.length-1) else word
          glyphMap.get(id) match {
            case None => warning(s"Reference $$$word to an unknown glyph")
              toGlyph(TextChunk(s"$$$id", textFont, fg, bg), fg)
            case Some(glyph) =>
              val copy = glyph()
              if (inPara) withBaseline(copy, (context.baseLine+copy.h)/2.3f) else copy
          }
        case word: String =>
          val glyph: Glyph = {
            // is there a discretionary hyphen // TODO: or more, eventually
            if (word.contains('​')) {
              val glyphs = word.toString.split("\u200B").toSeq.map { syllable => TextChunk(syllable, textFont, fg, bg).atBaseline(fg, bg) }
              val hyphen = TextChunk("-", textFont, fg, bg).atBaseline(fg, bg)
              new BreakableGlyph(hyphen, glyphs)
            }
            else
              toGlyph(TextChunk(word, textFont, fg, bg), fg)
          }
          glyph
      }
    }

    elem match {

      case Comment(_) =>
        List()

      case PCData(text) =>
        import context.{textFont, textBackgroundBrush => bg, textForegroundBrush => fg}
        import org.sufrin.glyph.{Text => TextChunk}

        val glyphs = text.split('\n').toSeq.map(TextChunk(_, textFont, fg, bg).asGlyph(fg, bg))
        List(NaturalSize.Col(bg=bg).atLeft$(glyphs))

      case EntityRef(id) =>
        import context.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        def textOf(text: String): Seq[Glyph] = List(toGlyph(TextChunk(text, textFont, fg=fg, bg=bg), fg))
        id match {
          case "amp" => textOf("&")
          case "ls" => textOf("<")
          case "gt" => textOf(">")
          case "nbsp" => textOf("\u00A0")
          case _ =>
            entityMap.get(id) match {
              case None =>
                warning(s"Unknown entity reference &$id;")
                textOf(s"&$id;")
              case Some(text) =>
                translateText(text)
            }
        }

      /** A text entity prefixed, and/or suffixed by some text, with no intervening space, is written
       *  as
       *  {{{ <entity>prefix&entityname;suffix<entity> }}}
       */
      case <embed>{child@_*}</embed> =>
        val (prefix, id, suffix) = child match {
          case Seq(Text(id)) => ("", id, "")
          case Seq(EntityRef(id)) => ("", id, "")
          case Seq(Text(l), EntityRef(id), Text(r)) => (l, id, r)
          case Seq(EntityRef(id), Text(r)) => ("", id, r)
          case Seq(Text(l), EntityRef(id)) => (l, id, "")
          case other =>
            warn(s"Bad <embed>")
            ("", "arising from bad <embed>", "")
        }
        entityMap.get(id) match {
          case None =>
            warning(s"Unknown entity reference $id")
            translateText(s"$prefix$suffix")
          case Some(text) =>
            translateText(s"$prefix$text$suffix")
        }


      case <s></s> =>
        List(FixedSize.Space(1f, context.parSkip, 0f))

      case <fill></fill> =>
        val stretch = localAttributes.Float("stretch", 1f)
        List(FixedSize.Space(1f, 1f, stretch))

      case <p>{child@_*}</p> =>
        val context$ = attributes.declareAttributes(context)
        val skip = context$.parSkip
        val thePara = decorated(glyphsToParagraph(child.flatMap { node => translate(sources$)(within$)(node)(attributes)(context$) })(context$))
        if (skip == 0.0f)
          List(thePara)
        else
          List(thePara above FixedSize.Space(1f, context.parSkip, 0f))

      case <verb>{child@_*}</verb> =>
        import org.sufrin.glyph.{Text => TextChunk}
        val context$ = attributes.declareAttributes(context)
        val local = context$
        val font = local.textFont
        val fg = local.textForegroundBrush
        val bg = local.textBackgroundBrush
        val background = attributes.Brush("bg", DefaultBrushes.nothing)
        val lines      = child.toString.split('\n').toSeq
        val lines$     = stripIndentation(lines)
        val texts      = lines$.map{ line => TextChunk(line, font, fg, bg).asGlyph()}
        List(decorated(NaturalSize.Col(bg=background).atLeft$(texts)))

      case <body>{child@_*}</body> =>
        val context$ = attributes.declareAttributes(context)
        val background = defaultAttributes.Brush("background", DefaultBrushes.nothing)
        import context$.{padX, padY}
        //context.xmlSource=attributes.String("source", "<unknown>")
        val glyphs = child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(decorated(NaturalSize.Col(bg=background).atLeft$(glyphs)).enlargedBy(padX, padY, bg=background))

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

      case <i>{child@_*}</i> =>
        val context$ = attributes.declareAttributes(context).italicStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <b>{child@_*}</b> =>
        val context$   = attributes.declareAttributes(context).boldStyle
        child.flatMap{  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <bi>{child@_*}</bi> =>
        val context$   = attributes.declareAttributes(context).boldItalicStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <n>{child@_*}</n> =>
        val context$   = attributes.declareAttributes(context).normalStyle
        child.flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }

      case <row>{child@_*}</row> if (localAttributes.get("width").isDefined) =>
        val context$ = attributes.declareAttributes(context)
        val width = localAttributes.Units("width", 0f)(context)
        val fg=context.textForegroundBrush
        val bg=context.textBackgroundBrush
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
        List(decorated(glyph))

      case <row>{child@_*}</row> =>
        val context$ = attributes.declareAttributes(context)
        val builder = NaturalSize.Row(fg=context.textForegroundBrush, bg=context.textBackgroundBrush)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "top"    => builder.atTop$(glyphs)
          case "bottom" => builder.atBottom$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/top/bottom [using 'top']\nat <${elem.label}${elem.attributes}>")
            builder.atTop$(glyphs)
        }
        List(decorated(glyph))

      case <col>{child@_*}</col> =>
        val context$ = attributes.declareAttributes(context)
        val builder = NaturalSize.Col(fg=context.textForegroundBrush, bg=context.textBackgroundBrush)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "left"    => builder.atLeft$(glyphs)
          case "right" => builder.atRight$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/left/right [using 'center']\nat <${elem.label}${elem.attributes}>")
            builder.centered$(glyphs)
        }
        List(decorated(glyph))

      case <table>{child@_*}</table> =>
        val context$ = attributes.declareAttributes(context)
        import context$.{padX, padY, textBackgroundBrush => bg, textForegroundBrush => fg}
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(decorated(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).table(height=height, width=width)(glyphs)))

      case <rows>{child@_*}</rows> =>
        val context$ = attributes.declareAttributes(context)
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        import context$.{padX, padY, textBackgroundBrush => bg, textForegroundBrush => fg}
        List(decorated(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).rows(width=width)(glyphs)))

      case <cols>{child@_*}</cols> =>
        val context$ = attributes.declareAttributes(context)
        import context$.{padX, padY, textBackgroundBrush => bg, textForegroundBrush => fg}
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(decorated(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).cols(height=height)(glyphs)))


      case <grid>{child@_*}</grid> =>
        val context$ = attributes.declareAttributes(context)
        import context$.{padX, padY, textBackgroundBrush => bg, textForegroundBrush => fg}
        val width = localAttributes.Int("columns", localAttributes.Int("cols", 0))
        val height = localAttributes.Int("rows", 0)
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        List(decorated(NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).grid(width=width, height=height)(glyphs)))

      case <glyph>{child@_*}</glyph> =>
        val context$ = attributes.declareAttributes(context)
        val id = localAttributes.String("ref", "")
        id match {
          case "" =>
            warning (s"Missing glyph reference <glyph>...")
            Seq.empty[Glyph]

          case _ =>
            glyphMap.get (id) match {
              case None =>
                warning (s"Unknown glyph reference <glyph ref=$id ...>...")
                Seq.empty[Glyph]
              case Some (glyph) =>
                val copy = glyph()
                List (decorated((if (inPara) withBaseline(copy, context.baseLine) else copy)))
            }
        }


      case elem: Elem =>
        val tag = elem.label
        generatorMap.get(tag) match {
          case None =>
            warning(s"Unknown tag <$tag ...")
            Seq.empty[Glyph]

          case Some(generator) =>
            List(decorated(generator(localAttributes, attributes.declareAttributes(context))))
        }

      // ZWSP unicodes are used at discretionary hyphen positions
      case Text(buffer) =>
        translateText(buffer)


      /** Identical to a lump of text */
      case elem  =>
        val buffer=elem.toString
        import context.{textFont, textForegroundBrush => fg}
        import org.sufrin.glyph.{Text => TextChunk}
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        buffer.toString.split("[\n\t ]+").toSeq.filterNot(_.isBlank).map{ word => toGlyph(TextChunk(word, textFont, fg, bg), fg) }

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