package org.sufrin.glyph

import Brush.SQUARE
import Glyphs.{BreakableGlyph, BUTT}
import GlyphTypes.{Font, FontStyle, Scalar}
import GlyphXML.{source, warn, AttributeMap}
import ReactiveGlyphs.{ColourButton, Reaction}
import Styles._
import Styles.Decoration.{Framed, Unframed}
import tests.DemonstrationNotebook.pageStyle

import org.sufrin.SourceLocation.SourceLocation

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._

object TestGXML extends Application {
  val translation = new GXML {}


  /**
   * Applied when an (outermost) xml `Elem`ent is intended to denote a `Glyph`. This
   * translates the element in a context that records its source location in scala text.
   */
  implicit def XML(elem: Elem)(implicit source: SourceLocation): Glyph = {
    val within = List("")
    NaturalSize.Col().atLeft$(translation.translate(List(s"$source"))(within)(elem)(Map.empty)(new Sheet()))
  }


  Brushes.buttonPointSize = 16
  val PRESSME = ReactiveGlyphs.TextButton("PRESS ME"){ _ => println("Pressed")}.framed()
  val QUIT    = ReactiveGlyphs.TextButton("QUIT"){ _ => println("Quit Pressed")}.framed()
  translation("PRESSME")  = PRESSME
  translation("QUIT")     = QUIT
  translation("TWIT")     = ReactiveGlyphs.TextButton("TWIT"){ _ => println("Twit Pressed")}.framed()
  translation("#p")       = ListMap("framed"->"true", "frame"->"red")
  translation("#row")     = ListMap("framed"->"true", "frame"->"blue")
  translation("unframed") = ListMap("framed"->"false", "frame"->"green/4")
  translation("RTWIT")    = ReactiveGlyphs.TextButton("TWIT"){ _ => println("Twit Pressed")}.rotated(3)
  translation("BUTTONS")  = NaturalSize.Row().centered(PRESSME.copy(), QUIT.copy()).rotated(2)

  val test0w = "50em"
  val test0 =
    <body fontFamily="Menlo" fontScale="1" fontSize="16" width={test0w} align="justify" parSkip="0.4ex" framed="0XFF227722/1" padX="3em" padY="3ex" background="yellow" textBackground="yellow">
      <p source={source.toString}>This is a test of  <i>paragraphing</i> and related features. It is justified in a width of {test0w},
          with an overall <row rotated="1"><b>parSkip</b></row> of <row rotated="2"><b>3.5ex</b></row>.
      </p>
      <p>
        This is some chatter to force the
        first part of a <b>paragraph</b> to the right edge.
        You are about to see an indent within the paragraph.
      </p>
      <p leftMargin="8em">
        This is a paragraph indented by 8em. It should still extend to the
        rightmost margin and be justified  there.
      </p>
      <row width={test0w}><fill/><glyph ref="PRESSME"/><fill/><glyph ref="QUIT"/></row>
      <p>This is an ordinary paragraph ending in Flocci​nauci​nihil​ipil​if​ication.</p>
      <p>Here is the button <glyph copy="true" ref="RTWIT"/> again!</p>
      <glyph ref="BUTTONS"/>
      <p rightMargin="6em">
        This is a paragraph narrowed by 6em. It should still
        be justified at its right margin.
      </p>
      <p leftMargin="12em" rightMargin="8em">
        This is another indented and potentially much-​hyphenated paragraph ending in the hyph​enated long word Flocci​nauci​nihil​ipil​if​ication.
      </p>
      <p leftMargin="8em" rightMargin="8em">
        This is a paragraph both indented and narr​owed by 8em.
        Its text should be right-justified, but as a whole it should appear obv​iously  centred
        in the space occupied by the paragraph.
      </p>
      <p align="right">
        This is some right-justified chatter. By right-justified I mean only that
        the extra space on a line appears at the start of the line.
      </p>
      <p align="center">
        This is some center-justified chatter. By center-justified I mean  that
        the extra space on a line is evenly divided between the start and the end of the line.
      </p>
    </body>


  val table1 =
    <table cols="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
      <col>5a 5b 5c</col>
      6 7 8
      &error;
      10 11
      <glyph ref="TWIT" copy="true"/>
    </table>
  translation("table1") = table1


  val table2 =
    <table rows="3" padY="2em" padX="2em" background="yellow" textBackground="yellow">
      1 2 3 4
      <col>5a 5b 5c</col>
      6 7 8 9 10 11
      <glyph ref="QUIT" copy="true"/>
    </table>
  translation("table2") = table2.rotated(1)

  val gridsWidth = s"${(table1.w + table2.h)*1.1f}pt"


  val test1 =
    <body width="50em" fontFamily="Menlo" fontScale="1" fontSize="16"
          align="justify" parSkip="0.4ex" padX="3em" padY="3ex" background="yellow" textBackground="yellow" class="unframed">
      <col class="unframed" align="center">
        <p class="unframed">Grids, with some deliberate errors in glyph references</p>
        <p class="unframed">FIX: buttons in rotated tables/grids/...</p>

        <row class="unframed" width={gridsWidth}  align="top">
        <fill/>
          <col class="unframed">
            <i>Columns: </i>
            $table1
          </col>
        <fill stretch="3"/>
          <col class="unframed">
            <i>Rows:</i>
            $table2
          </col>
        <fill/>
      </row>

        <!-- glyph ref="table1" copy="true"/ -->
      </col>
    </body>

  val noteBook: Notebook = Notebook()
  val Page: noteBook.DefinePage.type = noteBook.DefinePage

  Page("Paragraphs", "")(test0)
  Page("Grids", "")(test1)

  val title = "TestGXML"
  val GUI: Glyph = noteBook.Layout.rightButtons()
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
              textFontFamily  = FontFamily(String("fontFamily", context.textFontFamily.name)),
              textFontSize    = Float("fontSize",               context.textFontSize),
              fontScale       = Float("fontScale",              1.0f),
              // TODO: button and label font attributes
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
  def atBaseline(glyph: Glyph, baseLine$: Scalar): Glyph = new Glyph { thisGlyph =>
    locally { glyph.parent=thisGlyph }

    def draw(surface: Surface): Unit = {
      surface.withOrigin(0, -baseLine) {
        glyph.draw(surface)
      }
    }

    override def reactiveContaining(p: Vec): Option[ReactiveGlyph] =
      glyph.reactiveContaining(p-(0, baseLine))

    override def glyphContaining(p: Vec): Option[Hit] =
      glyph.glyphContaining(p-(0, baseLine))

    override def baseLine: Scalar = baseLine$

    def diagonal: Vec = Vec(glyph.w, glyph.h)

    def copy(fg: Brush=fg, bg: Brush=bg): Glyph = {
      org.sufrin.logging.Default.info(s"copying $glyph.atBaseline($baseLine)")
      atBaseline(glyph.copy(fg, bg), baseLine)
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

class GXML {
  import GXML._
  val glyphMap:     mutable.Map[String, Glyph] = mutable.LinkedHashMap[String, Glyph]()
  val generatorMap: mutable.Map[String, (AttributeMap, Sheet)=>Glyph] = mutable.LinkedHashMap[String, (AttributeMap, Sheet)=>Glyph]()
  val attrMap:      mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val sheetMap:     mutable.Map[String, Sheet] = mutable.LinkedHashMap[String, Sheet]()
  val reactionMap:  mutable.Map[String, Reaction] = mutable.LinkedHashMap[String, Reaction]()
  val entityMap:    mutable.Map[String, String] = mutable.LinkedHashMap[String, String]()

  def update(id: String, glyph: Glyph): Unit = glyphMap(id)=glyph
  def update(id: String, map: AttributeMap): Unit = attrMap(id)=map
  def update(id: String, reaction: Reaction): Unit = reactionMap(id)=reaction
  def update(id: String, generator: (AttributeMap, Sheet)=>Glyph) = generatorMap(id) = generator

  def glyph(id: String)(elem: xml.Elem)(implicit source: SourceLocation): Glyph = {
    val within = List("")
    val glyph = NaturalSize.Col().atLeft$(this.translate(List(s"$source"))(within)(elem)(Map.empty)(new Sheet()))
    glyphMap(id)=glyph
    glyph
  }

  def glyph(id: String)(glyph: Glyph): Glyph = {
    val within = List("")
    glyphMap(id)=glyph
    glyph
  }

  private val stylingTags: Seq[String] = List("b", "em", "i", "bi", "n", "hyph")
  def stylingTag(tag: String): Boolean = stylingTags.contains(tag)

  /**
   * Translate  `elem: Node` to the list of glyphs that it denotes.
   *
   * @param sources List of source locations of the nodes within which the `elem: Node` are nested.
   * @param within  List of source tags of the nodes within which the `elem: Node` are nested.
   * @param elem    The `Node` being translated.
   * @param inherited Map of the attributes inherited from the nodes within which `elem` is nested.
   * @param context   The style-sheet effective for the translation of `elem`.
   * @return The list of glyphs denoted by `elem`: sometimes a singleton list.
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
          case 1 | 3 => atBaseline(glyph.rotated(rotated), glyph.w)
          case 2     => atBaseline(glyph.rotated(rotated), glyph.h)
        }
      }

      def frame(glyph: Glyph): Glyph = {
        val brush = attributes.Brush("frame", DefaultBrushes.nothing)
        val framing = attributes.Bool("framed", brush.getAlpha != 0)
          if (framing)
            glyph.framed(fg = brush, bg = attributes.Brush("bg", glyph.bg))
          else
            glyph
        }

      rotate(frame(glyph))
    }


    def isBlank(elem: Node): Boolean = elem match {
      case Text(data) => data.isBlank
      case _ => false
    }


    elem match {

      case Comment(_) =>
        List()

      case PCData(text) =>
        import context.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        val glyphs = text.split('\n').toSeq.map(TextChunk(_, textFont, fg, bg).asGlyph(fg, bg))
        List(NaturalSize.Col.atLeft$(glyphs))

      case EntityRef(id) =>
        import context.{textBackgroundBrush, textFont, textForegroundBrush}
        import org.sufrin.glyph.{Text => TextChunk}
        val fg = textForegroundBrush
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        def textOf(text: String): Glyph = toGlyph(TextChunk(text, textFont, fg=fg, bg=bg), fg)
        val glyph = id match {
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
                textOf(text)
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
          List(decorated(ReactiveGlyphs.ColourButton(glyphs, down, hover, background)(act)))
        else
          List(decorated(ReactiveGlyphs.ColourButton(label, up, down, hover, bg, background)(act)))

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
        val background = attributes.Brush("background", DefaultBrushes.nothing)
        //TODO: maybe these are strictly local to body
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
                val copy = if (attributes.Bool("copy", false)) glyph.copy() else glyph
                List ((if (inPara) atBaseline(copy, context.baseLine) else copy))
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
        import context.{textFont, textForegroundBrush => fg}
        import org.sufrin.glyph.{Text => TextChunk}
        val bg = DefaultBrushes.nothing // To avoid the glyph outline extending below the bounding box of a glyph at the baseline
        buffer.toString.split("[\n\t ]+").toSeq.filterNot(_.isBlank).map {
          case s"$$$word" =>
            val copied = word.endsWith("()")
            val id     = if (copied) word.take(word.length-2) else word
            glyphMap.get(id) match {
              case None => warning(s"Reference $$$id to an unknown glyph")
                toGlyph(TextChunk(s"$$$id", textFont, fg, bg), fg)
              case Some (glyph) =>
                val copy = if (copied) glyph.copy() else glyph
                ((if (inPara) atBaseline(copy, context.baseLine) else copy))
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

case class Sheet
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
  // Paragraph layout properties
  parHang:     Boolean   = false,
  parAlign:    Alignment = Left,
  parSkip:     Scalar    = 5f,
  parWidth:    Scalar    = 200f,
  leftMargin:  Scalar=0f,
  rightMargin: Scalar=0f,
  parIndent:   ()=>Seq[Glyph] = { ()=>Nil },
  padX: Scalar = 0f,
  padY: Scalar = 0f,
  // Container constraints
  containerDimension: Vec = Vec.Zero
) {
  val core: Sheet = this

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
  def parNarrow(left: Scalar, right: Scalar): Sheet = {
    val lm = leftMargin
    val rm = rightMargin
    org.sufrin.logging.Default.info(s"parNarrow($left,$right) => ${(lm + left, rm+right)}")
    copy(leftMargin = lm + left, rightMargin = rm + right)
  }

  // derived
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

  lazy val unFramed: Sheet = new Sheet() {
    override lazy val buttonStyle: ButtonStyle = buttonStyle.copy(frame = Decoration.Unframed)
  }

  lazy val menuStyle: MenuStyle = MenuStyle(
    button = buttonStyle,
    nestedButton = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    reactive = buttonStyle.copy(frame = Framed(fg = DefaultBrushes.black(width = 0), bg = buttonBackgroundBrush)),
    inactive = Unframed,
    bg = DefaultBrushes.lightGrey,
    fg = DefaultBrushes.lightGrey,
  )

  lazy val emWidth: Scalar = textFont.measureTextWidth("m") // textFont.getMetrics.getMaxCharWidth//
  lazy val exHeight: Scalar = textFont.getMetrics.getXHeight //textFont.measureText("X").getHeight
  lazy val baseLine: Scalar = {
    val m = textFont.getMetrics
    m.getHeight+m.getDescent
  }

  @inline private def styled(fontStyle: FontStyle): Sheet = copy(textFontStyle = fontStyle)
  def italicStyle: Sheet = styled(FontStyle.ITALIC)
  def boldStyle: Sheet = styled(FontStyle.BOLD)
  def boldItalicStyle: Sheet = styled(FontStyle.BOLD_ITALIC)
  def normalStyle: Sheet = styled(FontStyle.NORMAL)

}



