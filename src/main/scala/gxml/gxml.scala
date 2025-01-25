package org.sufrin.glyph
package gxml


import io.github.humbleui.skija.Font
import org.sufrin.glyph.gxml.Visitor.AttributeMap
import org.sufrin.glyph.Glyphs.{BreakableGlyph, NOBREAK}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.GlyphXML.withBaseline

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._


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

    def substAttrs(attrs: MetaData, attrSubst: Map[String, String]): MetaData = {
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

class Primitives {
  val translationMap:       mutable.Map[String, Translation]  = mutable.LinkedHashMap[String, Translation]()
  val abstractionMap:       mutable.Map[String, Abstraction]  = mutable.LinkedHashMap[String, Abstraction]()
  val genericAttributesMap: mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val entityMap:            mutable.Map[String, String]       = mutable.LinkedHashMap[String, String]()
  val elementMap:           mutable.Map[String, Elem]         = mutable.LinkedHashMap[String, Elem]()
  val glyphMap:             mutable.Map[String, Glyph]        = mutable.LinkedHashMap[String, Glyph]()


  /** Declare a named glyph */
  def update(id: String, glyph: Glyph): Unit      = glyphMap(id)=glyph
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = genericAttributesMap(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: Translation) = translationMap(id) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = entityMap(id) = expansion
  /** Declare a new expandable entity */
  def update(id: String, element: Elem) : Unit =
    if (element.label.toUpperCase=="ATTRIBUTES")
      genericAttributesMap(id) = element.attributes.asAttrMap
    else
      elementMap(id) = element
  /** Declare a new macro */
  def update(id: String, abbr: Abstraction) : Unit = abstractionMap(id) = abbr
}

object Paragraph {

  def fromGlyphs(sheet: Sheet, glyphs: Seq[Glyph], parHang: Option[Glyph]): Glyph = {
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
}

object Translation {

  /**
   *   Translation takes place in stages: the `Target` stage
   *  is a convenient intermediate between GlyphXML notation and
   *  glyphs themselves. It's not strictly needed, but its objects
   *  are all prettyprintable, and it supports the debugging of
   *  complicated layouts.
   */
  object Target {
    trait Target {
      def asGlyph: Glyph
    }

    /** Joins its predecessor to its successor */
    case object JoinTarget extends Target with PrettyPrint.PrettyPrintable {
      /** The name of the class (or object) */
      def prefix: String = "Join"

      /** The number of fields/elements of the object */
      def arity: Int = 0

      val asGlyph: Glyph = NOBREAK
    }

    /** The text of a word or a line */
    case class TextTarget(text: String, atBase: Boolean, font: Font, fg: Brush, bg: Brush) extends Target with PrettyPrint.PrettyPrintable {
      val arity = 5
      val prefix = "Text"

      override def field(i: Int): (String, Any) = i match {
        case 1 => ("atBase", atBase)
        case 0 => ("", s""""$text"""")
        case 2 => ("font", FontFamily.fontString(font))
        case 3 => ("fg", fg)
        case 4 => ("bg", bg)
      }

      val asGlyph: Glyph = {
        val source = org.sufrin.glyph.Text(text, font, fg, bg)
        if (atBase) source.atBaseline() else source.asGlyph()
      }
    }

    /** The structure of a paragraph to be made from chunks */
    case class ParaTarget(sheet: Sheet, chunks: Seq[Target], parHang: Option[Glyph]) extends Target with PrettyPrint.PrettyPrintable
    { val arity=3
      val prefix="Para"
      override def field(i: Int): (String, Any) = i match {
        case 2 => ("chunks", chunks)
        case 1 => ("", s"w: ${sheet.parWidth}")
        case 0 => ("hang", parHang)
      }
      val asGlyph: Glyph = Paragraph.fromGlyphs(sheet, chunks.map(_.asGlyph), parHang)
    }

    case class ColTarget(background: Brush, chunks: Seq[Target]) extends Target {
      val asGlyph: Glyph = NaturalSize.Col(bg=background).atLeft$(chunks.map(_.asGlyph))
    }

    case class GlyphTarget(paragraph: Boolean, sheet: Sheet, glyph: Glyph) extends Target with PrettyPrint.PrettyPrintable {
      val arity=2
      val prefix="GlyphTarget"
      override def field(i: Int): (String, Any) = i match {
        case 1 => ("glyph", glyph)
        case 0 => ("para",  paragraph)
      }

      val glyph$ = if (paragraph) withBaseline(glyph, sheet.baseLine) else glyph
      val asGlyph: Glyph = glyph$
    }

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

  }

  def isBlank(elem: Node): Boolean = elem match {
    case Text(data) => data.isBlank
    case _ => false
  }
}

class TypedAttributeMap(attributes: AttributeMap) {
  import org.sufrin.SourceLocation.SourceLocation
  import org.sufrin.logging.SourceDefault.{warn}

  def String(key: String, alt: String): String = attributes.getOrElse(key, alt)

  def Int(key: String, alt: Int)(implicit source: SourceLocation): Int = attributes.get(key) match {
    case Some (s) if s.matches("-?[0-9]+") => s.toInt
    case Some(s)  =>
      warn(s"$key(=$s) should be an Int [using $alt]")(source)
      alt
    case None     => alt
  }

  def Float(key: String, alt: Float)(implicit source: SourceLocation): Float = attributes.get(key) match {
    case Some (s) =>
      try { s.toFloat }
      catch {
        case exn: Throwable  => org.sufrin.logging.Default.warn(s"$key(=$s) should be a Float [using $alt]")
          alt
      }
    case None     => alt
  }

  def Units(key: String, alt: Float)(attributes: AttributeMap, sheet: Sheet)(implicit source: SourceLocation): Float = attributes.get(key) match {
    case Some(spec) =>
      spec.toLowerCase match {
        case (s"${s}em") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * sheet.emWidth
        case (s"${s}ex") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat * sheet.exHeight
        case (s"${s}px") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
        case (s"${s}pt") if s.matches("[0-9]+(\\.([0-9]+)?)?") => s.toFloat
        case (other) =>
          warn(s"$key(=$other) should specify its unit of measure in em/ex/px/pt")
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
    case None => default
    case Some(alignment) => alignment.toLowerCase match {
      case ("left") => Left
      case ("right") => Right
      case ("center") => Center
      case ("justify") => Justify
      case (other) =>
        warn(s"$key=\"$other\" [not an alignment name: using \"center\"]")
        Center
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
          val brush = DefaultBrushes.namedColour(name)
          brushCache(name)=brush
          brush
      }
  }

  /**
   * Context derived from `attributes` by declaring
   * the universally-applicable attributes in
   * for the `Node`.
   */
  def declareAttributes(sheet: Sheet): Sheet = {
    val fontDetail: Sheet =
      sheet
        .copy(
          fontScale       = Float("fontScale",              1.0f),

          textFontStyle   = FontFamily.styleNamed(String("textStyle", "")),
          textFontFamily  = FontFamily(String("fontFamily", String("textFontFamily", sheet.textFontFamily.name))),
          textFontSize    = Float("fontSize", Float("textFontSize", sheet.textFontSize)),

          labelFontStyle  = FontFamily.styleNamed(String("labelStyle", "")),
          labelFontFamily = FontFamily(String("fontFamily", String("labelFontFamily", sheet.labelFontFamily.name))),
          labelFontSize   = Float("fontSize", Float("labelFontSize", sheet.labelFontSize)),

          buttonFontStyle   = FontFamily.styleNamed(String("buttonStyle", "")),
          buttonFontFamily = FontFamily(String("fontFamily", String("buttonFontFamily", sheet.buttonFontFamily.name))),
          buttonFontSize   = Float("fontSize", Float("buttonFontSize",                  sheet.buttonFontSize)),
        )

    // Units are computed relative to the font details, which may have been redeclared
    fontDetail.copy(
      padX                  = Units("padX",           sheet.padX)         (attributes, fontDetail),
      padY                  = Units("padY",           sheet.padY)         (attributes, fontDetail),
      parWidth              = Units("width",          sheet.parWidth)     (attributes, fontDetail),
      parSkip               = Units("parSkip",        sheet.parSkip)      (attributes, fontDetail),
      leftMargin            = Units("leftMargin",     sheet.leftMargin)   (attributes, fontDetail),
      rightMargin           = Units("rightMargin",    sheet.rightMargin)  (attributes, fontDetail),
      parAlign              = Align("align",          sheet.parAlign),
      textBackgroundBrush   = Brush("textBackground", sheet.textBackgroundBrush),
      textForegroundBrush   = Brush("textForeground", sheet.textForegroundBrush),
      buttonBackgroundBrush = Brush("buttonBackground", sheet.buttonBackgroundBrush),
      buttonForegroundBrush = Brush("buttonForeground", sheet.buttonForegroundBrush),
      labelBackgroundBrush  = Brush("labelBackground", sheet.labelBackgroundBrush),
      labelForegroundBrush  = Brush("labelForeground", sheet.labelForegroundBrush),
    )
  }
}

class Translation(primitives: Primitives)  {
  import Visitor.AttributeMap
  import Translation.Target._
  import primitives._

  implicit class TypedMap(attributes: AttributeMap) extends TypedAttributeMap(attributes)

  def extendContextFor(tag: String)(inherited: AttributeMap, local: Map[String,String]): AttributeMap = inherited ++ local

  def translateText(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {

    @inline def wordGlyph(text: String): Target = TextTarget(text, paragraph, sheet.textFont, sheet.textForegroundBrush, DefaultBrushes.nothing)

    if (paragraph) {
      // Generate the target chunks of texts;  with `JoinTarget` before (after) unless the text as a whole starts with a space, tab, or newline
      val chunks = mutable.ArrayBuffer[Target]()
      def out(t: Target): Unit =
          t match {
            case JoinTarget =>
              if (chunks.nonEmpty && chunks.last != JoinTarget) chunks += t
            case _ =>
              chunks += t
          }

      if (text.startsWith(" ") || text.startsWith("\n") || text.startsWith("\t")) {} else out(JoinTarget)

      val it = text.split("[\t\n ]+").filterNot(_.isBlank).iterator // TODO: precompile the pattern
      while (it.hasNext) {
        out(wordGlyph(it.next()))
      }
      if (text.endsWith(" ") || text.endsWith("\n") || text.endsWith("\t")) {} else out(JoinTarget)
      chunks.toSeq
    } else {
      val lines = text.split("[\n]").map(_.trim).map(wordGlyph(_)).toSeq
      // List(WordGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).atLeft$(lines), sheet.textFontStyle.toString))
      List(ColTarget(sheet.backgroundBrush, lines ))
    }
  }

  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    child.flatMap { source => translate(tags, paragraph, attributes, sheet$, source) }
  }

  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, source: Node): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)

    source match {
      case xml.EntityRef(name) => translateText(tags, paragraph, attributes, sheet$, entityMap.getOrElse(name, s"&$name;"))

      case xml.Elem(str, tag, metaData, binding, child@_*) =>

        val localAttributes = metaData.asAttrMap

        def attrsFor(attrId: String): AttributeMap = {
          localAttributes.get(attrId) match {
            case None       => Map.empty
            case Some(attr) =>
              genericAttributesMap.get(attr) match {
                case None =>
                  import org.sufrin.logging.Default.warn
                  warn(s"No attribute defaults for $attrId=\"$attr\" in ${tags.reverse.mkString("<", "<", "...>")} ")
                  Map.empty

                case Some(attrs) =>
                  //org.sufrin.logging.Default.info(s"attrsFor($attrId) $attr = $attrs")
                  attrs
              }
          }
        }

        /**
         * The inherited attributes of an element with tag `label` are the catenation of the
         * globally-declared attributes for `tag:label`, then those of its declared "class",
         * then those of its specific "id".
         */
        val specifiedAttributes: AttributeMap = genericAttributesMap.getOrElse(s"tag:${tag}", Map.empty) ++ attrsFor("class") ++ attrsFor("id")

        /**
         * The effective attributes of an element are the catenation of its default attributes and its actually-appearing
         * attributes, without "id" and "class"
         */
        val attributes$$ : AttributeMap = specifiedAttributes ++ localAttributes.filterNot{ case (key, _) => key=="class" || key=="id"}
        val attributes$ : AttributeMap = specifiedAttributes ++ localAttributes
        val sheet$ = attributes$.declareAttributes(sheet)
        val tags$ = tag::tags
        tag match {
          case "p" =>
            println(s"<p ${Visitor.toString(attributes$)}")
            val hangString = attributes$.String("hang", "")
            val hang = if (hangString.isEmpty) None else Some(sheeted.Label(hangString)(sheet$))
            val chunks = child.flatMap { source => translate(tags$, true, attributes$$, sheet$, source) }
            List(ParaTarget(sheet$, chunks, hang))

          case "glyph" =>
            println (s"<glyph ${Visitor.toString(attributes$)}/>")
            val id: String     = attributes$.String("id", alt="")
            val copy: Boolean  = attributes$.Bool("copy", !attributes$.Bool("share", true))
            val fg             = attributes$.Brush("fg",   DefaultBrushes.black)
            val bg             = attributes$.Brush("bg",   DefaultBrushes.nothing)
            glyphMap.get(id)  match {
              case None =>
                translateText(tags$, paragraph, attributes$, sheet$, s"<glyph $id ${Visitor.toString(attributes$)}")
              case Some(glyph: Glyph) =>
                List(GlyphTarget(paragraph, sheet$, if (copy) glyph(fg, bg) else glyph))
            }



          case _ =>
            translationMap.get(tag) match {
              case Some(translation) =>
                println(s"<$tag special translation ${Visitor.toString(attributes$)}")
                translation.translate(tags$, paragraph, attributes$, sheet$, child)

              case None =>
                //println(s"<$tag default translation  ${Visitor.toString(attributes$)}")
                //child.flatMap { source => translate(tags$, false, attributes$, sheet$, source) }
                translatePCData(tags$, false, Map.empty, Sheet(), source.toString())
            }

        }

      case xml.Text(text) =>
           translateText(tags, paragraph, attributes, sheet, text)

      case xml.PCData(text: String) =>
           translatePCData(tags, paragraph, attributes, sheet$, text)

      case xml.ProcInstr(target, text) =>
           translateProcInstr(tags, target, text)

      case xml.Comment(text) => Seq.empty

    }

  }

  /** Perhaps better to introduce a PCData target .... */
  def translatePCData(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {
    import sheet.{textFont, textBackgroundBrush => bg, textForegroundBrush => fg}
    import org.sufrin.glyph.{Text => TextChunk}
    val glyphs = text.split('\n').toSeq.map(TextChunk(_, textFont, fg, bg).asGlyph(fg, bg))
    List(GlyphTarget(paragraph, sheet, NaturalSize.Col(bg = bg).atLeft$(glyphs)))
  }


  def translateProcInstr(tags: List[String], target: String, text: String): Seq[Target] = Seq.empty
}


object gxml extends Application {
  import xml._
  import Translation.Target._

  val primitives: Primitives = new Primitives
  locally {
    primitives("i")  = textStyleTranslation("i", "Italic")
    primitives("b")  = textStyleTranslation("b", "Bold")
    primitives("bi") = textStyleTranslation("bi", "BoldItalic")
    primitives("n")  = textStyleTranslation("n", "Normal")
    primitives("today") = " Expansion of today "
    primitives("yesterday") = " Expansion of yesterday "
    primitives("body") = new Translation(primitives) {
      override def toString: String = "body translation"
      override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
        val child$ = child.filterNot(Translation.isBlank(_))
        List(ColTarget(sheet.backgroundBrush, chunks=super.translate("body"::tags, false, attributes, sheet, child$)))
      }
    }
    implicit val sheet = Sheet(labelForegroundBrush = DefaultBrushes.green)
    import sheeted.{Label,TextButton}
    primitives("B1")     = Label("B1")
    primitives("B2")     = Label("B2") // TextButton("UNSHARED") { _=> }
    primitives("B3")     = Label("B3") // TextButton("UNSHARED") { _=> }
    primitives("B1")       = <ATTRIBUTES fg="red/2"       copy="true" bg="yellow"/>
    primitives("B2")       = <ATTRIBUTES fg="lightGrey/2" copy="true" bg="green"/>
    primitives("B3")       = <ATTRIBUTES fg="lightGrey/60" share="true"/>
    primitives("tag:p")    = <ATTRIBUTES background="black/60" leftMargin="2em"/>
  }


  def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(primitives) {
    override def toString: String = s"StyleTranslation($tag, $textStyle)"
    override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
      super.translate(tag::tags, paragraph, attributes.updated("textStyle", textStyle), sheet, child)
    }
  }

  val translator: Translation = new Translation(primitives)


  def translate(source: Node): Glyph = {
    import PrettyPrint.AnyPretty
    for { t <- (translator.translate(Nil, false, Map.empty, Sheet(), source)) } t.prettyPrint()
    NaturalSize.Col()
               .centered$(translator.translate(Nil, false, Map.empty, Sheet(), source).map(_.asGlyph)).framed()
  }


  val h: Node  =
    <body xmlns:h="http://www.w3.org/TR/html4/" width="40em" face="Courier">
      the first body line
      <div class="level1" width="2000px">
        <p> this&today;is a very<i>interesting</i>'ed
            paragraph
            <bi>and this is bold italic text</bi><foo/>
            <![CDATA[
            and
            this
            is
            cdata]]>
            with <i>some</i>more text
        </p>
        a <i>completely</i> level1 line
      </div>
      the last body lump&yesterday;what?
      <p>
        Several buttons:
        <glyph id="B1"/> and
        <glyph id="B2"/> and
        <glyph id="B3"/>
        <glyph id="B3" copy="true" bg="yellow"/>
        <glyph id="B3" copy="false" bg="yellow"/>

      </p>
    </body>

  val p1: Node =
    <body  width="40em" textFontFamily="Menlo" textFontSize="18">

      <glyph id="B2"/>


      <p align="justify" leftMargin="20em">
         The rain in spain falls <i>mainly</i> in the plain. <glyph id="B1"/>
         Oh! Does it? <b>Oh</b>, Yes!, it does. &yesterday;  eh? &today;
      </p>

      <glyph id="B1"/>

      <p textBackground="yellow" leftMargin="0em" rightMargin="20em">
        The rain in spain falls <i>mainly</i> in the plain.
        Oh! Does it? <b>Oh, </b>Yes!, it does.
        Why do<bi>-you-</bi>want to control spacing so tightly? Here&yesterday;we go!
      </p>

    This is what happens
    to material  <glyph id="B1"/> outside
    paragraph boundaries

      <p><i>the wage of gin is <and fg="red">
        this is
         <what> happens
         </what>
        when a tag is not defined
      </and> BREATH!</i></p>

      <![CDATA[
      And this is a lump
      of ![CDATA ..]]
      OK?
      ]]>
    </body>

  def GUI: Glyph = translate(p1)

  def title: String = "gxml"
}

