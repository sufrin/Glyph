package org.sufrin.glyph


import Glyphs.{BreakableGlyph, NOBREAK}
import GlyphTypes.Scalar
import ReactiveGlyphs.{ColourButton, Reaction}

import org.sufrin.glyph.GlyphXML.AttributeMap
import org.sufrin.logging.SourceLoggable

import scala.::
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._

object GlyphXML extends SourceLoggable {
  type AttributeMap = Map[String,String]

  implicit class Attributes(element: Elem) {
    def mapping: Map[String,String] = element.attributes.asAttrMap
  }

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

    val brushCache = collection.mutable.HashMap[String, Brush]()

    def Brush(key: String, alt: Brush): Brush = attributes.get(key) match {
      case None       => alt
      case Some(name) =>
        brushCache.get(name) match {
          case Some(brush) =>
            brush
          case None =>
            val brush = DefaultBrushes.namedColour(name)
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

            textFontStyle   = FontFamily.styleNamed(String("textStyle", "")),
            textFontFamily  = FontFamily(String("fontFamily", String("textFontFamily", context.textFontFamily.name))),
            textFontSize    = Float("fontSize", Float("textFontSize", context.textFontSize)),

            labelFontStyle   = FontFamily.styleNamed(String("labelStyle", "")),
            labelFontFamily = FontFamily(String("fontFamily", String("labelFontFamily", context.labelFontFamily.name))),
            labelFontSize   = Float("fontSize", Float("labelFontSize", context.labelFontSize)),

            buttonFontStyle   = FontFamily.styleNamed(String("buttonStyle", "")),
            buttonFontFamily = FontFamily(String("fontFamily", String("buttonFontFamily", context.buttonFontFamily.name))),
            buttonFontSize   = Float("fontSize", Float("buttonFontSize",                  context.buttonFontSize)),
          )

      // Units are computed relative to the font details, which may have been redeclared
      fontDetail.copy(
        padX                  = Units("padX",           context.padX)         (fontDetail),
        padY                  = Units("padY",           context.padY)         (fontDetail),
        parWidth              = Units("width",          context.parWidth)     (fontDetail),
        parSkip               = Units("parSkip",        context.parSkip)      (fontDetail),
        leftMargin            = Units("leftMargin",     context.leftMargin)   (fontDetail),
        rightMargin           = Units("rightMargin",    context.rightMargin)  (fontDetail),
        parAlign              = Align("align",          context.parAlign),
        textBackgroundBrush   = Brush("textBackground", context.textBackgroundBrush),
        textForegroundBrush   = Brush("textForeground", context.textForegroundBrush),
        buttonBackgroundBrush = Brush("buttonBackground", context.buttonBackgroundBrush),
        buttonForegroundBrush = Brush("buttonForeground", context.buttonForegroundBrush),
        labelBackgroundBrush  = Brush("labelBackground", context.labelBackgroundBrush),
        labelForegroundBrush  = Brush("labelForeground", context.labelForegroundBrush),
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
   *
   *  TODO: Aligment for hanging material should be specifiable. Right now it's always "left"
   *  TODO: Solve the mystery of the space-expansion within "short last lines" of paragraphs in justify mode,
   *        when a nested paragraph-form appears immediately afterwards (see SheetTest).
   *
   */
  def glyphsToParagraph(glyphs: Seq[Glyph], parHang: Option[Glyph])(sheet: Sheet): Glyph = {
    val emWidth   = sheet.emWidth
    val interWord = FixedSize.Space(w=emWidth / 1.5f, h=0f, stretch = 2f)
    val glyphs$   =
      (if (sheet.parIndent>0) List(FixedSize.Space(sheet.parIndent, h=0f, stretch=0f)) else Nil) ++ glyphs

    val (hangGlyph, hangWidth) = parHang match {
      case None    => (None, 0f)
      case Some(h) => (Some(h), h.w)
    }

    val leftMargin = sheet.leftMargin max hangWidth


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

    hangGlyph match {
      case None =>
        if (leftMargin > 0f)
          NaturalSize.Row(bg = sheet.textBackgroundBrush)
            .centered(
              FixedSize.Space(w =  leftMargin, h = 0f, stretch = 0f),
              column,
              FixedSize.Space(w = sheet.rightMargin, h = 0f, stretch = 0f))
        else
          column

      case Some(theGlyph) =>
        val space = FixedSize.Space(leftMargin-theGlyph.w,theGlyph.h, 0f)
        NaturalSize.Row(bg = sheet.textBackgroundBrush)
          .centered(
            NaturalSize.Row(bg = sheet.textBackgroundBrush).atTop(theGlyph, space, column),
            FixedSize.Space(w = sheet.rightMargin, h = 0f, stretch = 0f))
    }
  }

  /**
   * Represents a primitive macro whose parameters are extracted from the body of the macro call, and substituted in the
   * body of the definition.
   *
   * TODO: Eventually this could be made more general, by making substitution more sophisticated
   *
   * <someabstraction>....</someabstraction> is reduced to `body` with `....` substituted for all instances of `&BODY;` in it. If `....` consists of
   * more than just a single node, then node `i` is substituted for all instances of `&BODYi;`. If `....` is empty, no substitutions are made.
   * @param body -- the body of the abstraction.
   */
  class Abstraction(body: Node) {
    def expansion(invocation: Node): Seq[Node] = {
      val children = invocation.child.filterNot{
        case Text(t) => t.isBlank
        case _ => false
      }

      val actuals: IterableOnce[(String,Node)] =
        if (children.isEmpty)
          List("BODY"-> <!-- --> ) // the BODY is empty
        else
          children.take(1).map{ ("BODY"-> _) } ++ (0 until children.length).map(i=>(s"BODY$i"->children(i)))

      val bindings = new mutable.HashMap[String,Node]()
      bindings.addAll(actuals)
      // TODO:  ListMap appears to have no effective constructor from lists of pairs -- at least none that IntelliJ Scala accepts

      def substitute(node: Node): Node = {
        node match {
          case EntityRef(id) =>
            if (!bindings.contains(id) && id.startsWith("BODY")) org.sufrin.logging.Default.warn(s"Abstraction reference $invocation has no $id in $body")
            bindings.get(id) match {
              case None             => node
              case Some(node: Elem) => substitute(node)
              case Some(other)      => other
            }
          case elem: Elem =>
            elem.copy(child = elem.child.map(substitute(_)), attributes = substAttrs(elem.attributes, invocation.attributes.asAttrMap))
          case _ =>
            node
        }
      }

      def substAttrs(attrs: MetaData, attrSubst: AttributeMap): MetaData = {
        def deref(value: String): String = {
          val result = {
            value match {
              case s"$$$ref($value)" =>
                attrSubst.get(ref) match {
                  case Some(value) => value
                  case None        => value
                }
              case s"$$$ref" =>
                attrSubst.get(ref) match {
                  case Some(value) => value
                  case None =>
                    org.sufrin.logging.Default.warn(s"Abstraction reference $invocation has no parameter $ref")
                    ""
                }
              case _ => value
            }
          }
          //println(s"$value ---> $result")
          result
          }

        var attribs: MetaData = Null
        for { attr <- attrs } {
            attribs = MetaData.concatenate(attribs, Attribute(null, attr.key, deref(attr.value.text), attribs))//MetaData.concatenate(attribs, attr.prefixedKey, attr.value.text)
        }

        attribs = MetaData.normalize(attribs, null)
        //println(s"$attrs => $attribs")
        attribs
      }

      val result = substitute(body)
      //println(s"$invocation\n => $result")
      result
    }
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
            line += interWord()
            lineWidth += words.element.w + interWordWidth
            words.nextElement()

          // REMOVE THE IMMEDIATELY PRECEDING INTERWORD SPACE
          case NOBREAK =>
            val w = line.last.w
            line.update(line.length - 1, NOBREAK)
            lineWidth -= w
            words.nextElement()

          case other =>
            line += other
            line += interWord()
            lineWidth += words.element.w + interWordWidth
            words.nextElement()
        }
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

trait ElementGenerator {
  def translate(xml: GlyphXML)(sources: List[String])(within: List[String])(child: Seq[Node])(localAttributes: AttributeMap)(context: Sheet): Seq[Glyph]
}

/**
 * The semantic specification of XML that denotes a glyph.
 */
class GlyphXML {
  import GlyphXML._

  val thisGlyphXML: GlyphXML = this

  private trait GlyphGen {
    def generate: Glyph
  }
  private case class Literal(glyph: Glyph) extends GlyphGen {
    def generate: Glyph = glyph
  }
  private case class Generator(generator: ()=>Glyph) extends GlyphGen {
    def generate: Glyph = generator()
  }



  // Mappings from names (used within XML) to relevant glyphs or their aspects.
  private val glyphMap:       mutable.Map[String,GlyphGen] = mutable.LinkedHashMap[String,GlyphGen]()
  private val generatorMap:   mutable.Map[String, ElementGenerator] = mutable.LinkedHashMap[String, ElementGenerator]()
          val attrMap:        mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  private val sheetMap:       mutable.Map[String, Sheet] = mutable.LinkedHashMap[String, Sheet]()
  private val reactionMap:    mutable.Map[String, Reaction] = mutable.LinkedHashMap[String, Reaction]()
  private val entityMap:      mutable.Map[String, String] = mutable.LinkedHashMap[String, String]()
  private val abstractionMap: mutable.Map[String, Abstraction] = mutable.LinkedHashMap[String, Abstraction]()
  private val elementMap:     mutable.Map[String, Elem] = mutable.LinkedHashMap[String, Elem]()

  /** Declare a named glyph */
  def update(id: String, glyph: Glyph): Unit       = glyphMap(id)=Literal(glyph)
  def update(id: String, glyph: () => Glyph): Unit = glyphMap(id)=Generator(glyph)
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = attrMap(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: ElementGenerator) = generatorMap(id) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = entityMap(id) = expansion
  /** Declare a new expandable entity */
  def update(id: String, element: Elem) : Unit =
      if (element.label.toUpperCase=="ATTRIBUTES")
        attrMap(id) = element.attributes.asAttrMap
      else
        elementMap(id) = element
  /** Declare a new macro */
  def update(id: String, abbr: Abstraction) : Unit = abstractionMap(id) = abbr

  locally {
      entityMap ++= List("amp" -> "&", "ls"-> "<", "gt" -> ">", "nbsp" -> "\u00A0" )
  }


  private val stylingTags: Seq[String] = List("b", "em", "i", "bi", "n", "hyph", "glyph", "splice", "use", "nb")
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
   *               The "words" will (in  general) be set with spaces between them. There is no systematic workaround, though the
   *               notation <nobreak>....</nobreak> treats its body (....) as a single chunk of text with no breaks.
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
              //org.sufrin.logging.Default.info(s"attrsFor($attrId) $attr = $attrs")
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

    def textOf(text: String): Seq[Glyph] = {
      import context.{textBackgroundBrush=>bg, textFont, textForegroundBrush}
      import org.sufrin.glyph.{Text => TextChunk}
      List(toGlyph(TextChunk(text, textFont, fg=DefaultBrushes.red, bg=bg), fg=DefaultBrushes.red))
    }

    def expandDollar(word: String): Seq[Glyph] = {
      import context.{textFont, textForegroundBrush => fg}
      import org.sufrin.glyph.{Text => TextChunk}
      val id = if (word endsWith "$") word.substring(0, word.length-1) else word
      val bg = DefaultBrushes.nothing
      glyphMap.get(id) match {
        case Some(glyph) =>
          val copy = glyph.generate
          List(if (inPara) withBaseline(copy, (context.baseLine+copy.h)/2.3f) else copy)

        case None => elementMap.get(id) match {
            case Some(node) =>  translate(sources$)("use" :: within)(node)(attributes)(context)

            case None       =>
                 entityMap.get(id) match {
                   case None =>
                     warning(s"Unknown glyph/macro reference $id")
                     textOf(s"$$$word")

                   case Some(text) =>
                     translateText(text)

                 }
        }
      }
    }

    def translateText(buffer: String): Seq[Glyph] = {
      import context.discretionaryWordBreak
      import context.{textFont, textForegroundBrush => fg}
      import org.sufrin.glyph.{Text => TextChunk}
      val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
      val text = buffer.toString
      val textChunks = text.split("[\n\t ]+").toSeq
      val glyphs = textChunks.filterNot(_.isBlank).flatMap {
        case s"$$$word" =>
            expandDollar(word)
        case word: String =>
          val glyph = {
            // is there a discretionary hyphen // TODO: or more, eventually
            if (word.contains(discretionaryWordBreak)) {
              val glyphs = word.toString.split(discretionaryWordBreak).toSeq.map { syllable => TextChunk(syllable, textFont, fg, bg).atBaseline(fg, bg) }
              val hyphen = TextChunk("-", textFont, fg, bg).atBaseline(fg, bg)
              new BreakableGlyph(hyphen, glyphs)
            }
            else
              toGlyph(TextChunk(word, textFont, fg, bg), fg)
          }
          List(glyph)
      }
      // TODO: eventually add NOBREAKs in the right places
      glyphs
    }

    def paragraphGlyphs(sources: List[String])(within: List[String])(elements: Seq[Node])(inherited: AttributeMap)(context: Sheet): Seq[Glyph] = {
      val out  = mutable.ArrayBuffer[Glyph]()
      val in = elements.iterator
      while (in.hasNext) {
        in.next() match {
          case Text(buf) =>
            val leftGap = buf.startsWith(" ") || buf.startsWith("\n") || buf.startsWith("\t")
            val rightGap = buf.endsWith(" ") || buf.endsWith("\n") || buf.endsWith("\t")
            val text = buf.strip
            if (!leftGap) out.addOne(NOBREAK)
            out.addAll(translateText(text))
            if (!rightGap) out.addOne(NOBREAK)

          case <b>{child@_*}</b> =>
            val context$   = attributes.declareAttributes(context).boldStyle
            //child.flatMap{  node => translate(sources$)(within$)(node)(attributes)(context$) }
            out.addAll(paragraphGlyphs(sources)("b"::within)(child)(inherited)(context$))

          case other =>
            out.addAll(translate(sources)(within)(other)(inherited)(context))
        }
      }
      out.toSeq
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


        elementMap.get(id) match {
          case Some(node) =>
            //println(s"Expanding $id as $node ")
            translate(sources$)("use" :: within)(node)(attributes)(context)

          case None =>
            entityMap.get(id) match {
              case None =>
                warning(s"Unknown entity reference &$id;")
                textOf(s"&$id;")
              case Some(text) =>
                translateText(text)
            }
        }


      /** Simple macro */
      case <use>{child@_*}</use> =>
        val id = localAttributes.String("ref", "")
        expandDollar(id)

      case <splice>{child@_*}</splice> =>
        val context$ = attributes.declareAttributes(context)
        child.flatMap { elt => translate(sources$)(within$)(elt)(localAttributes)(context$) }

      /**
       * Glue together children without intervening spaces.
       */
      case <string>{child@_*}</string> =>
        val buffer = new StringBuffer()
        for { elt <- child } elt match {
          case EntityRef(id) => buffer append entityMap.getOrElse(id, "")
          case Text(t) => buffer append t.strip()
          case other => buffer append other.toString.strip()
        }
        import context.{textFont, textForegroundBrush => fg, textBackgroundBrush => bg}
        import org.sufrin.glyph.{Text => TextChunk}
        List(toGlyph(TextChunk(buffer.toString, textFont, fg=fg, bg=bg), fg))


      case <nb>{child@_*}</nb> =>
        if (child.isEmpty)  List(NOBREAK) else {
          val context$ = attributes.declareAttributes(context)
          val glyphs = child.flatMap { node => translate(sources$)(within$)(node)(attributes)(context$) }
          val row = NaturalSize.Row.atTop$(glyphs)
          List((withBaseline(row, row.baseLine)))
        }


      case <s></s> =>
        List(FixedSize.Space(1f, context.parSkip, 0f))

      case <fill></fill> =>
        val stretch = localAttributes.Float("stretch", 1f)
        List(FixedSize.Space(1f, 1f, stretch))

      case <p>{child@_*}</p> =>
        val context$ = attributes.declareAttributes(context)
        val skip = context$.parSkip



        val hangString = attributes.String("hang", "")
        val hang = if (hangString.isEmpty) None else Some(sheeted.Label(hangString)(context$))

        val glyphs = paragraphGlyphs(sources$)(within$)(child)(attributes)(context$)
        val thePara = decorated(glyphsToParagraph(glyphs, hang)(context$))
        if (skip == 0.0f)
          List(thePara)
        else
          List(thePara above FixedSize.Space(1f, context.parSkip, 0f))

      case <verb>{child@_*}</verb> =>
        import org.sufrin.glyph.{Text => TextChunk}
        val context$ = attributes.declareAttributes(context)
        val local = context$
        val font = local.textFont
        val fg = context$.textForegroundBrush
        val bg = context$.textBackgroundBrush
        val background = attributes.Brush("bg", DefaultBrushes.nothing)
        val lines      = child.toString.split('\n').toSeq
        val lines$     = stripIndentation(lines)
        val texts      = lines$.map{ line => TextChunk(line, font, fg, bg).asGlyph(fg, bg)}
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
        val glyphs =
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
        val align = localAttributes.String("align", "")
        align match {
          case "center" => List(decorated(NaturalSize.Col(bg=attributes.Brush("bg", DefaultBrushes.nothing)).centered$(glyphs)))
          case "left" => List(decorated(NaturalSize.Col(bg=attributes.Brush("bg", DefaultBrushes.nothing)).atLeft$(glyphs)))
          case "right" => List(decorated(NaturalSize.Col(bg=attributes.Brush("bg", DefaultBrushes.nothing)).atRight$(glyphs)))
          case _ => glyphs
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

      case <col>{child@_*}</col> if (localAttributes.get("height").isDefined) =>
        val context$ = attributes.declareAttributes(context)
        val height = localAttributes.Units("width", 0f)(context)
        val fg=context.textForegroundBrush
        val bg=context.textBackgroundBrush
        val glyphs = child.filterNot(isBlank(_)).flatMap {  node => translate(sources$)(within$)(node)(attributes)(context$) }
        val builder  = FixedSize.Col.apply(height, fg=fg, bg=bg)
        val glyph = attributes.String("alignment", "center") match {
          case "center" => builder.centered$(glyphs)
          case "left"   => builder.atLeft$(glyphs)
          case "right"  => builder.atRight$(glyphs)
          case other =>
            warning(s"alignment(=$other) should be center/left/right [using 'top']\nat <${elem.label}${elem.attributes}>")
            builder.centered$(glyphs)
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
                val copy = glyph.generate
                List (decorated((if (inPara) withBaseline(copy, context.baseLine) else copy)))
            }
        }


      case elem: Elem =>
        val tag = elem.label
        abstractionMap.get(tag) match {
          case Some(abstraction) =>
               val expanded = abstraction.expansion(elem)
               val context$ = localAttributes.declareAttributes (context)
               expanded.flatMap{ elt => translate(sources)(within)(elt)(localAttributes)(context$) }

          case None => generatorMap.get (tag) match {
                case None =>
                warning (s"Unknown tag <$tag ...")
                Seq.empty[Glyph]

                case Some (generator) =>
                val context$ = attributes.declareAttributes (context)
                generator.translate (thisGlyphXML) (sources$) (within$) (elem.child) (localAttributes) (context$)

                }
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