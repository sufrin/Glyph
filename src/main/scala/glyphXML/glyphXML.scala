package org.sufrin.glyph
package glyphXML


import io.github.humbleui.skija.Font
import org.sufrin.glyph.glyphXML.Translation.AttributeMap
import org.sufrin.glyph.Glyphs.{BreakableGlyph, INVISIBLE}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.glyphXML.Translation.isBlank
import org.sufrin.glyph.sheeted.windowdialogues.Dialogue

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.xml._


class Abstraction(body: Node) {

  def expansion(invocationAttributes: AttributeMap, invocation: Node): Seq[Node] = {

    val children = invocation.child.filterNot{
      case Text(t) => t.isBlank
      case _ => false
    }

    val actuals: IterableOnce[(String,Seq[Node])] =
      if (children.isEmpty)
        List("BODY"-> <!-- --> ) // the BODY is empty
      else
        (0 until children.length).map(i=>s"BODY$i"->List(children(i)))

    val bindings = new mutable.HashMap[String,Seq[Node]]()
    bindings.addAll(actuals)
    bindings.addAll(List("BODY"->children))
    // TODO:  ListMap appears to have no effective constructor from lists of pairs -- at least none that IntelliJ Scala accepts

    def substitute(node: Node): Seq[Node] = {
      node match {
        case EntityRef(id) =>
          if (!bindings.contains(id) && id.startsWith("BODY")) org.sufrin.logging.Default.warn(s"Abstraction reference $invocation has no $id in $body")
          bindings.get(id) match {
            case None             => List(node)
            case Some(nodes)      => nodes.flatMap(substitute(_))
          }
        case elem: Elem =>
          elem.copy(child = elem.child.flatMap(substitute(_)), attributes = substAttrs(elem.attributes, invocationAttributes/*invocation.attributes.asAttrMap*/))
        case _ =>
          node
      }
    }

    def substAttrs(attrs: MetaData, actualAttributes: Map[String, String]): MetaData = {
      def deref(value: String): String = {
        val result = {
          value match {
            case s"$$$ref($value)" =>
              actualAttributes.get(ref) match {
                case Some(value) => value
                case None        => value
              }
            case s"$$$ref" =>
              actualAttributes.get(ref) match {
                case Some(value) => value
                case None =>
                  org.sufrin.logging.Default.warn(s"Abstraction reference $invocation has no parameter $ref")
                  ""
              }
            case _ => value
          }
        }
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
    val logging = invocationAttributes.getOrElse("logging", "")
    if (logging.nonEmpty) {
      org.sufrin.logging.Default.info(s"Expanding $logging  ($body)")
      org.sufrin.logging.Default.info(s"  $invocation")
      org.sufrin.logging.Default.info(s"  $result")
    }

    result
  }
}

class Primitives {
  val translationMap:       mutable.Map[String, Translation]  = mutable.LinkedHashMap[String, Translation]()
  val abstractionMap:       mutable.Map[String, Abstraction]  = mutable.LinkedHashMap[String, Abstraction]()
  val genericAttributesMap: mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val entityMap:            mutable.Map[String, String]       = mutable.LinkedHashMap[String, String]()
  val elementMap:           mutable.Map[String, Elem]         = mutable.LinkedHashMap[String, Elem]()
  val generatorMap:         mutable.Map[String, Sheet=>Glyph] = mutable.LinkedHashMap[String, Sheet=>Glyph]()

  private def restore[K,V](saved: (mutable.Map[K,V], Seq[(K,V)])): Unit = {
    val (original, copy) = saved
    original.clear()
    original.addAll(copy)
  }

  private def save[K,V](map: mutable.Map[K,V]): (mutable.Map[K,V], Seq[(K,V)]) = (map, map.toSeq)

  def scoped[V](body: =>V) = {
    val (m1, m2, m3, m4, m5, m6) = (
      save(translationMap),
      save(abstractionMap),
      save(genericAttributesMap),
      save(entityMap),
      save(elementMap),
      save(generatorMap)
    )
    val result = body
    restore(m1)
    restore(m2)
    restore(m3)
    restore(m4)
    restore(m5)
    restore(m6)
    result
  }

  /** Declare a named glyph generator */
  def update(id: String, glyph: Sheet=>Glyph): Unit = generatorMap(id)=glyph
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = genericAttributesMap(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: Translation) = translationMap(id) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = entityMap(id) = expansion
  /** Declare a new expandable entity */
  def update(id: String, node: Node) : Unit = {
    node  match {
      case element: Elem =>
        if (element.label.toUpperCase == "ATTRIBUTES")
          genericAttributesMap(id) = Translation.normalizeKeys(element.attributes.asAttrMap)
        else
          elementMap(id) = element

      case other =>
        import org.sufrin.logging.Default.error
        error(s"Meaningless XML assignment ...($id) = $node")
    }
  }

  /** Declare a new macro */
  def update(id: String, abbr: Abstraction) : Unit = abstractionMap(id) = abbr
}

object Paragraph {

  def fromGlyphs(sheet: Sheet, glyphs: Seq[Glyph], parHang: Option[Glyph]): Glyph = {
    val glyphs$   =
      (if (sheet.parIndent>0) List(Glyphs.Rect(sheet.parIndent, 1f, fg=DefaultBrushes.nothing)) else Nil) ++ glyphs

    val (hangGlyph, hangWidth) = parHang match {
      case None    => (None, 0f)
      case Some(h) => (Some(h), h.w)
    }

    val leftMargin = sheet.leftMargin max hangWidth


    // The overall width is determined by the context
    // If the bounding box is unspecified, then use the column width
    val galley =
      formatParagraph(
        overallWidth   = sheet.parWidth - hangWidth,
        align          = sheet.parAlign,
        leftMargin     = sheet.leftMargin,
        rightMargin    = sheet.rightMargin,
        interWordWidth = sheet.emWidth,
        glyphs$
      )

    val column = NaturalSize.Col(bg = sheet.textBackgroundBrush).atLeft$(galley.toSeq)


    hangGlyph match {
      case None =>
        if (true || leftMargin > 0f)
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
  def formatParagraph(overallWidth:   Scalar,
                      align:          Alignment,
                      leftMargin:     Scalar,
                      rightMargin:    Scalar,
                      interWordWidth: Scalar,
                      glyphs:         Seq[Glyph]) = {
    // As each line of the paragraph is assembled it is added to the galley
    val galley = ArrayBuffer[Glyph]()
    // maximum width of this paragraph: invariant
    val maxWidth       = overallWidth - (leftMargin + rightMargin)
    // avoid rounding
    val maxWidthfloor  = maxWidth.floor
    //println(s"[ov=$overallWidth,lm=$leftMargin,maxw=$maxWidthfloor]")
    val words = new StreamIterator[Glyph](glyphs.iterator)

    @inline def setLine(): (Scalar, Seq[Glyph]) = {
      import scala.collection.mutable
      val line = mutable.IndexedBuffer[Glyph]()
      var lineWidth = 0f

      // Skip any leading interwordspaces left over from a previous line.
      while (words.hasElement && words.element.isInstanceOf[FixedSize.Space]) words.nextElement()

      // start the line
      line += align.leftFill()
      // add words and interword spaces while there is room
      while (words.hasElement && (lineWidth + words.element.w < maxWidthfloor)) {
      words.element match {
          case BreakableGlyph(_, glyphs) =>
            for { glyph <- glyphs } line += glyph
            lineWidth += words.element.w
            words.nextElement()

          case other =>
            line += other
            lineWidth += words.element.w
            words.nextElement()
        }
      }

      // squeeze an extra chunk on by splitting a breakable?
      if (words.hasElement) words.element match {
        case breakable: BreakableGlyph =>
          val breakPoint: Int = breakable.maximal(maxWidthfloor-lineWidth-interWordWidth-breakable.hyphen.w) //??
          if (breakPoint!=0) {
            val glyphs = breakable.glyphs
            for { i <- 0 until breakPoint } {
              lineWidth += glyphs(i).w
              line += glyphs(i)
            }
            line += breakable.hyphen()
            //line += interWord()// (from the earlier implementation
            lineWidth += breakable.hyphen.w
            words.buffer = Some(new BreakableGlyph(breakable.hyphen, glyphs.drop(breakPoint)))
          }
        case _ =>
      }


      // line is full
      val endLine = if (words.hasElement) align.rightFill() else align.lastFill()
      // terminate the made-up line with a line-ending stretchy space
      if (line.last.isInstanceOf[FixedSize.Space])
        line.update(line.length - 1, endLine)
      else
        line+=endLine

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
            galley += NaturalSize.Row(Glyphs.FilledRect(maxWidthfloor, other.h, fg = DefaultBrushes.red(width=2)))
        }
        words.nextElement()
      } else {
        val (width, glyphs) = setLine()
        // the line had only its starting alignment glyph; nothing else to do
        if (glyphs.length == 1 && width == 0) {
          setting = false
        } else
          galley += FixedSize.Row(maxWidth).atBottom$(glyphs)
      }
    }
    galley
  }
}

object Translation {

  type AttributeMap=Visitor.AttributeMap

  def normalizeKeys(map: AttributeMap): AttributeMap = map

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
    case class InterwordTarget(width: Scalar) extends Target {
      val asGlyph: Glyph = new FixedSize.Space(width, 1f, 1f, 0f)
    }

    /** The text of a word or a line that will not be broken at a hyphen */
    case class SolidTextTarget(text: String, atBase: Boolean, font: Font, fg: Brush, bg: Brush) extends Target with PrettyPrint.PrettyPrintable {
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

    /** The text of a word or a line that could be broken at a hyphen */
    case class HyphenatableTextTarget(text: String, discretionaryWordBreak: String, font: Font, fg: Brush, bg: Brush) extends Target with PrettyPrint.PrettyPrintable {
      val arity = 5
      val prefix = "Text"
      override def field(i: Int): (String, Any) = i match {
        case 1 => ("-", discretionaryWordBreak)
        case 0 => ("", s""""$text"""")
        case 2 => ("font", FontFamily.fontString(font))
        case 3 => ("fg", fg)
        case 4 => ("bg", bg)
      }
      import org.sufrin.glyph.{Text=>makeText}
      val asGlyph = if (text.contains(discretionaryWordBreak)){
        val glyphs = text.toString.split(discretionaryWordBreak).toSeq.map { syllable =>makeText(syllable, font, fg, bg).atBaseline() }
        val hyphen =makeText("-", font, fg, bg).atBaseline()
        new BreakableGlyph(hyphen, glyphs)
      }
      else makeText(text, font, fg, bg).atBaseline()

    }

    /** The structure of a paragraph to be made from `targets` */
    case class ParaTarget(sheet: Sheet, targets: Seq[Target], parHang: Option[Glyph]) extends Target with PrettyPrint.PrettyPrintable
    { val arity=3
      val prefix="Para"
      override def field(i: Int): (String, Any) = i match {
        case 2 => ("chunks", targets)
        case 1 => ("", s"w: ${sheet.parWidth}")
        case 0 => ("hang", parHang)
      }
      val asGlyph: Glyph = Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)
    }

    case class ColTarget(background: Brush, chunks: Seq[Target], alignment: Alignment = Left) extends Target {
      val theGlyphs = chunks.map(_.asGlyph)
      val theGlyph =  alignment match {
        case Justify | Right  => NaturalSize.Col(bg=background).atRight$(theGlyphs)
        case Center => NaturalSize.Col(bg=background).centered$(theGlyphs)
        case _      => NaturalSize.Col(bg=background).atLeft$(theGlyphs)
      }
      val asGlyph: Glyph = theGlyph
    }

    case class GlyphTarget(paragraph: Boolean, sheet: Sheet, glyph: Glyph) extends Target with PrettyPrint.PrettyPrintable {
      val arity=2
      val prefix="GlyphTarget"
      override def field(i: Int): (String, Any) = i match {
        case 1 => ("glyph", glyph)
        case 0 => ("para",  paragraph)
      }

      val glyph$ = glyph // if (paragraph) withBaseline(glyph, glyph.h/2) else glyph //**
      val asGlyph: Glyph = glyph$
    }

    case class DecorateTarget(tags: Seq[String], paragraph: Boolean, attributes: TypedAttributeMap, target: Target) extends Target {
      def raiseBy(glyph: Glyph, by: Scalar): Glyph = glyph // withBaseline(glyph, by)

      def decorated(glyph: Glyph): Glyph = {
        val g0 = glyph
        def rotate(glyph: Glyph): Glyph = {
          val rotated = attributes.Int("rotated", 0)
          if (rotated==0) glyph else
          if (!paragraph)
            glyph.rotated(rotated) else
          rotated % 4 match {
            case 0     => glyph
            case 1 | 3 => raiseBy(glyph.rotated(rotated), glyph.w)
            case 2     => raiseBy(glyph.rotated(rotated), 2*glyph.h)
          }
        }

        def turn(glyph: Glyph): Glyph =
          attributes.Int("turned", 0) match {
            case 0 => glyph
            case d =>
              val glyph$ = glyph.turned(d.toFloat, false)
              glyph$//atBaseline(glyph$, glyph$.h)
          }

        def frame(glyph: Glyph): Glyph = {
          val brush = attributes.Brush("frame", DefaultBrushes.invisible)
          val framing = attributes.Bool("framed", brush.getAlpha != 0)
          if (framing)
            glyph.framed(fg = brush, bg = attributes.Brush("bg", glyph.bg))
          else
            glyph
        }

        def scale(glyph: Glyph): Glyph = {
          val scale: Scalar = attributes.Float("scaled", 1f)
          if (scale!=1f) glyph.scaled(scale) else glyph
        }

        scale(turn(frame(rotate(glyph))))
      }

      val asGlyph: Glyph = decorated(target.asGlyph)

    }

    /**
     *
     */
    def withBaseline(glyph: Glyph, baseLine$: Scalar): Glyph = new Glyph { thisGlyph =>

      locally { glyph.parent=thisGlyph }

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

      def draw(surface: Surface): Unit = glyph.draw(surface)
    }

  }

  def isBlank(elem: Node): Boolean = elem match {
    case Text(data) => data.isBlank
    case _ => false
  }
}

class TypedAttributeMap(unNormalized: AttributeMap) {
  import org.sufrin.SourceLocation.SourceLocation
  import org.sufrin.logging.SourceDefault.{warn}

  val attributes: AttributeMap = unNormalized // .map{ case (k,d) => (k.toLowerCase, d)}

  def asString: String = Visitor.toString(attributes)
  override def toString: String = Visitor.toString(attributes)

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

  def Units(key: String, alt: Float)(attributes: AttributeMap, sheet: Sheet)(implicit source: SourceLocation): Float = {
    attributes.get(key) match {
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
  }

  def Bool(key: String, default: Boolean): Boolean = attributes.get(key) match {
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
        warn(s"$key=\"$other\" [not a horizontal alignment name: using \"center\"]")
        Center
    }
  }

  def VAlign(key: String, default: VAlignment): VAlignment = attributes.get(key) match {
    case None => default
    case Some(alignment) => alignment.toLowerCase match {
      case ("top") => Top
      case ("bottom") => Bottom
      case ("mid") => Mid
      case ("center") => Mid
      case (other) =>
        warn(s"$key=\"$other\" [not a vertical alignment name: using \"mid\"]")
        Mid
    }
  }



  def Brush(key: String, alt: Brush): Brush = attributes.get(key) match {
    case None       => alt
    case Some(name) => DefaultBrushes(name)
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

          buttonFontStyle  = FontFamily.styleNamed(String("buttonStyle", "")),
          buttonFontFamily = FontFamily(String("fontFamily", String("buttonFontFamily", sheet.buttonFontFamily.name))),
          buttonFontSize   = Float("fontSize", Float("buttonFontSize",                  sheet.buttonFontSize)),
        )

    // Units are computed relative to the font details, which may have been redeclared
    fontDetail.copy(
      padX                  = Units("padX",           sheet.padX)         (attributes, fontDetail),
      padY                  = Units("padY",           sheet.padY)         (attributes, fontDetail),
      parWidth              = Units("width",          sheet.parWidth)     (attributes, fontDetail),
      parSkip               = Units("parSkip",        sheet.parSkip)      (attributes, fontDetail),
      parIndent             = Units("parIndent",      sheet.parIndent)    (attributes, fontDetail),
      leftMargin            = Units("leftMargin",     sheet.leftMargin)   (attributes, fontDetail),
      rightMargin           = Units("rightMargin",    sheet.rightMargin)  (attributes, fontDetail),
      parAlign              = Align("align",          sheet.parAlign),
      backgroundBrush       = Brush("background",     sheet.backgroundBrush),
      foregroundBrush       = Brush("foreground",     sheet.foregroundBrush),
      textBackgroundBrush   = Brush("textBackground", sheet.textBackgroundBrush),
      textForegroundBrush   = Brush("textForeground", sheet.textForegroundBrush),
      buttonBackgroundBrush = Brush("buttonBackground", sheet.buttonBackgroundBrush),
      buttonForegroundBrush = Brush("buttonForeground", sheet.buttonForegroundBrush),
      labelBackgroundBrush  = Brush("labelBackground", sheet.labelBackgroundBrush),
      labelForegroundBrush  = Brush("labelForeground", sheet.labelForegroundBrush),
    )
  }
}

class Translation(val primitives: Primitives=new Primitives) {
  import Visitor.AttributeMap
  import Translation.Target._
  import primitives._
  implicit class TypedMap(attributes: AttributeMap) extends TypedAttributeMap(attributes)
  val meaning: Translation = this

  import primitives.entityMap
  /** Declare a named glyph generator */
  def update(id: String, glyph: Sheet=>Glyph): Unit = primitives(id)=glyph
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = primitives(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: Translation) = primitives(id) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = primitives(id) = expansion
  locally { entityMap ++= List("quot" -> "\"", "apos" -> "'", "amp" -> "&", "ls"-> "<", "gt" -> ">", "nbsp" -> "\u00A0" ) }

  /** Declare a new expandable entity; or a named collection of attributes */
  def update(id: String, element: Node) : Unit = primitives(id) = element


  /** Declare a new macro */
  def update(id: String, abbr: Abstraction) : Unit = primitives(id) = abbr

  def extendContextFor(tag: String)(inherited: AttributeMap, local: Map[String,String]): AttributeMap = inherited ++ local

  def translateText(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {

    @inline def solidText(text: String): Target = SolidTextTarget(text, paragraph, sheet.textFont, sheet.textForegroundBrush, DefaultBrushes.nothing)
    @inline def hyphenatableText(text: String): Target = HyphenatableTextTarget(text, sheet.discretionaryWordBreak, sheet.textFont, sheet.textForegroundBrush, DefaultBrushes.nothing)

    val interWordWidth = sheet.interWordWidth

    if (paragraph) {
      // Generate the target chunks of texts;  with `JoinTarget` before (after) unless the text as a whole starts with a space, tab, or newline
      val chunks = mutable.ArrayBuffer[Target]()
      def out(t: Target): Unit =
          t match {
            //case _: InterwordTarget =>
            //  if (chunks.nonEmpty && !chunks.last.isInstanceOf[InterwordTarget]) chunks += t
            case _ =>
              chunks += t
          }

      if (text.startsWith(" ") || text.startsWith("\n") || text.startsWith("\t")) out(InterwordTarget(interWordWidth))
      val it = text.split("[\t\n ]+").filterNot(_.isBlank).iterator // TODO: precompile the pattern
      while (it.hasNext) {
        out(hyphenatableText(it.next()))
        if (it.hasNext) out(InterwordTarget(interWordWidth))
      }
      if (text.endsWith(" ") || text.endsWith("\n") || text.endsWith("\t")) out(InterwordTarget(interWordWidth))
      chunks.toSeq
    } else {
      val lines = text.split("[\n]").map(_.trim).filterNot(_.isEmpty).map(solidText(_)).toSeq
      // List(WordGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).atLeft$(lines), sheet.textFontStyle.toString))
      List(ColTarget(sheet.backgroundBrush, lines ))
    }
  }

  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, children: Seq[Node]): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    children.flatMap { source => translate(tags, paragraph, attributes, sheet$, source) }
  }

  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, source: Node): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    def tagString:String = tags.reverse.mkString("<", "<", "")

    source match {
      case xml.EntityRef(name) => translateText(tags, paragraph, attributes, sheet$, entityMap.getOrElse(name, s"&$name;"))

      case xml.Elem(str, tag, metaData, binding, children@_*) =>

        val localAttributes = Translation.normalizeKeys(metaData.asAttrMap)

        def attrsFor(attrId: String): AttributeMap = {
          localAttributes.get(attrId) match {
            case None       => Map.empty
            case Some(attr) =>
              genericAttributesMap.get(attr) match {
                case None =>
                  import org.sufrin.logging.Default.warn
                  warn(s"$tagString No attribute defaults for $attrId=\"$attr\" in ${tags.reverse.mkString("<", "<", "")} ")
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
         * The effective attributes of an element are the catenation of its default attributes and its actually appearing
         * attributes, without "id" and "class"
         */
        val attributes$$ : AttributeMap = specifiedAttributes ++ localAttributes.filterNot{ case (key, _) => key.toLowerCase=="class" || key.toLowerCase=="id"}
        val attributes$ : AttributeMap = specifiedAttributes ++ localAttributes
        val sheet$ = attributes$.declareAttributes(sheet)
        val tags$ = tag::tags

        def Decorated(t: Target): Seq[Target] = List(DecorateTarget(tags$, paragraph, attributes$, t))

        tag match {
          case "p" =>
            //println(s"<p ${attributes$.asString} parIndent=${sheet$.parIndent}")
            val hangString = attributes$.String("hang", "")
            val hangRef    = attributes$.String("hangid", "")
            val hang: Option[Glyph] =
              if (hangString.isEmpty)
                 if (hangRef.isEmpty)
                   None
                 else
                   Some(generateGlyph(tags$, paragraph, attributes$, sheet$, hangRef))
              else Some(sheeted.Label(hangString)(sheet$))
            val chunks = children.flatMap { source => translate(tags$, true, attributes$$, sheet$, source) }
            Decorated(ParaTarget(sheet$, chunks, hang))

          case "div" | "body" =>
            val children$ = children.filterNot(Translation.isBlank(_))
            Decorated(ColTarget(sheet.backgroundBrush, chunks = translate(tags$, false, attributes$, sheet$, children$)))

          case "col" =>
            val glyphs: Seq[Target] = children.filter(_.isInstanceOf[Elem]).flatMap { source => translate(tags$, paragraph, attributes$$, sheet$, source) }
            val alignment = attributes$.Align("align", Left)
            Decorated(ColTarget(sheet$.backgroundBrush, glyphs, alignment))


          case "row" =>
            val inheritWidth = attributes$.Bool("inheritwidth", false)
            val defaultWidth = if (inheritWidth) sheet$.parWidth else 0f
            val width = attributes$.Units("width", defaultWidth)(attributes$, sheet$)
            val glyphs = children.filter(_.isInstanceOf[Elem]).flatMap { source => translate(tags$, false, attributes$$, sheet$, source) }.map(_.asGlyph)
            val valign: VAlignment = attributes$.VAlign("valign", Top)
            val glyph = if (width==0f)
                 NaturalSize.Row(align=valign, bg=sheet$.backgroundBrush)(glyphs)
            else
                 FixedSize.Row(align=valign, width=width, bg=sheet$.backgroundBrush)(glyphs)
            Decorated(GlyphTarget(paragraph, sheet$, glyph))

          case "fill" =>
            val width  = attributes$.Units("width", sheet$.emWidth)(attributes$, sheet$)
            val height = attributes$.Units("height", sheet$.exHeight)(attributes$, sheet$)
            val stretch = attributes$.Float("stretch", 1f)
            val background = attributes$.Brush("background", attributes$.Brush("bg", DefaultBrushes.nothing))
            Decorated(GlyphTarget(paragraph, sheet$, FixedSize.Space(width, height, stretch, fg=DefaultBrushes.nothing, bg=background)))

          case "glyph" =>
            //println (s"<glyph ${attributes$.asString}/>")
            val id: String = localAttributes.String("gid", alt="")
            if (generatorMap.get(id).isDefined)
               Decorated(GlyphTarget(paragraph, sheet$, generateGlyph(tags$, paragraph, attributes$, sheet$, id)))
            else {
               elementMap.get(id) match {
                 case None =>
                   org.sufrin.logging.Default.warn (s"$tagString <glyph ${attributes.asString}/> UNDEFINED (no generator or element)")
                   Decorated(GlyphTarget(paragraph, sheet$, sheeted.Label (s"UNDEFINED $id")(sheet$.copy(labelForegroundBrush = DefaultBrushes.red))))
                 case Some(element) =>
                   translate(tags, paragraph, attributes, sheet, element)
               }
            }

          case "table" =>
            import sheet$.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
            val width     = attributes$.Int("columns", attributes$.Int("cols", 0))
            val height    = attributes$.Int("rows", 0)
            val uniform   = attributes$.Bool("uniform", false)
            val glyphs    = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, paragraph, attributes$, sheet$, source) }.map(_.asGlyph)
            //println(s"$padX, $padY $fg, $bg")
            val Grid      = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY)
            val buildFrom = if (uniform) Grid.grid(height=height, width=width)(_) else Grid.table(height=height, width=width)(_)
            Decorated(GlyphTarget(paragraph = paragraph,
                                  sheet = sheet$,
                                  glyph = buildFrom(glyphs)))

          case "rows" =>
            import sheet$.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
            val width  = attributes$.Int("columns", attributes$.Int("cols", 0))
            val height = attributes$.Int("rows", 0)
            val glyphs = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, paragraph, attributes$, sheet$, source) }.map(_.asGlyph)
            //println(s"$padX, $padY")
            Decorated(GlyphTarget(paragraph = paragraph,
              sheet = sheet$,
              glyph = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).rows(width=width)(glyphs)))

          case "cols" =>
            import sheet$.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
            val width  = attributes$.Int("columns", attributes$.Int("cols", 0))
            val height = attributes$.Int("rows", 0)
            val glyphs = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, paragraph, attributes$, sheet$, source) }.map(_.asGlyph)
            //println(s"$padX, $padY")
            Decorated(GlyphTarget(paragraph = paragraph,
              sheet = sheet$,
              glyph = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY).cols(height=height)(glyphs)))

          case "span" =>
            children.flatMap { child => translate(tags$, paragraph, attributes$, sheet$, child) }

          case "SCOPE" =>
            primitives.scoped(children.flatMap { child => translate(tags$, paragraph, attributes$, sheet$, child) })

          case "ATTRIBUTES" =>
            // Set the attributes defined for the given id.
            // The scope of this is the body of the closest enclosing nonempty <ATTRIBUTES></ATTRIBUTES>; but is global if the body is empty.
            val id = localAttributes.String("key", "")
            val attrs = attributes$$.removedAll(List("key"))
            genericAttributesMap(id)=attrs
            val attributes$$$=attributes.removedAll(List("key", "class"))
            val result = children.flatMap { child => translate(tags$, paragraph, attributes$$$, sheet$, child) }
            result

          case "ENTITY" =>
            // Set the attributes defined for the given id.
            // The scope of this is the body of the closest enclosing nonempty <ATTRIBUTES></ATTRIBUTES>; but is global if the body is empty.
            val id = localAttributes.String("key", "")
            val expansion = localAttributes.String("expansion", "")
            entityMap(id)=expansion
            val attributes$$$=attributes$.removedAll(List("id", "expansion"))
            val result = children.flatMap { child => translate(tags$, paragraph, attributes$$$, sheet$, child) }
            result

          case "ELEMENT" =>
            // Set the attributes defined for the given id.
            // The scope of this is the body of the closest enclosing nonempty <ATTRIBUTES></ATTRIBUTES>; but is global if the body is empty.
            val id = localAttributes.String("key", "")
            val children$ = children.filterNot(isBlank)
            val definition = children$.take(1)
            val body = children$.drop(1)
            if (definition.isEmpty)
              Seq.empty
            else {
              elementMap(id) = definition.head.asInstanceOf[Elem]
              val attributes$$$ = attributes$.removedAll(List("key"))
              val result = body.flatMap { child => translate(tags$, paragraph, attributes$$$, sheet$, child) }
              result
            }

          case "attributes" =>
            import org.sufrin.logging.Default.info
            info(s"${tagString}<attributes ${attributes$.asString} ${if (paragraph) "(P)" else ""}")
            Seq.empty


          case _ =>
            translationMap.get(tag) match {
              case Some(translation) =>
                //println(s"<$tag special translation ${Visitor.toString(attributes$)}")
                translation.translate(tags$, paragraph, attributes$, sheet$, children)

              case None =>
                abstractionMap.get(tag) match {
                  case None =>
                    translatePCData(tags$, false, Map.empty, Sheet(), source.toString())
                  case Some(abstraction) =>
                    //println(s"Abstraction ${attributes$.asString}")
                    val expanded: Seq[Node] = abstraction.expansion(attributes$, source)
                    translate(tags$, paragraph, attributes$, sheet$, expanded)
                }
            }

        }

      case xml.Text(text) =>
           translateText(tags, paragraph, attributes, sheet, text)

      case xml.PCData(text: String) =>
           translatePCData(tags, paragraph, attributes, sheet$, text)

      case xml.ProcInstr(target, text) =>
           translateProcInstr(tags, target, text)

      case xml.Comment(text) => Seq.empty

      case _ =>
        // private representation as a scala.xml.Atom
        translateText(tags, paragraph, attributes, sheet, s" $source ")

    }

  }

  /** Generate a glyph from the given glyph `ref`erence  */
  private def generateGlyph(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, ref: String): Glyph =
    generatorMap.get(ref) match {
      case None =>
            org.sufrin.logging.Default.warn (s"<glyph ${attributes.asString}/> UNDEFINED (no generator)")
            sheeted.Label (s"UNDEFINED $ref") (sheet.copy (labelForegroundBrush = DefaultBrushes.red) )
      case Some(glyph) =>
        glyph(sheet)
    }

  private def leadingSpaces(s: String): Int = {
    var n = 0
    while (n<s.length && s(n)==' ') n+=1
    n
  }

  private val indentationOrdering: Ordering[String] = new Ordering[String] {
    def compare(x: String, y: String): Int = leadingSpaces(x)-leadingSpaces(y)
  }

  def stripCommonIndentation(lines: Seq[String]): Seq[String] = {
    val lines$ = lines.dropWhile(_.isBlank)
    val prefix = leadingSpaces(lines$.min(indentationOrdering))
    if (lines$.last.isBlank)
      lines$.init.map(_.substring(prefix))
    else
      lines$.map(_.substring(prefix))
  }

  /** Perhaps better to introduce a PCData target .... */
  def translatePCData(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {
    import sheet.{textFont, textBackgroundBrush => bg, textForegroundBrush => fg}
    import org.sufrin.glyph.{Text => makeText}
    val rawLines = text.split('\n').toSeq
    val lines    = if (attributes.Bool("normalizePCData", true)) stripCommonIndentation(rawLines) else rawLines
    val glyphs   = lines.map(makeText(_, textFont, fg, bg).asGlyph(fg, bg))
    List(GlyphTarget(paragraph, sheet, NaturalSize.Col(bg = bg).atLeft$(glyphs)))
  }


  def translateProcInstr(tags: List[String], target: String, text: String): Seq[Target] = Seq.empty

  /** Make this translation accessible */
  def apply(source: Node)(implicit sheet: Sheet): Glyph = {
    val translator = this
    //import PrettyPrint.AnyPretty
    //for { t <- (translator.translate(Nil, false, Map.empty, sheet, source)) } t.prettyPrint()
    NaturalSize.Col()
      .centered$(translator.translate(Nil, false, Map.empty, sheet, source).map(_.asGlyph))
  }

  implicit def XMLNodetoGlyph(source: Node)(implicit sheet: Sheet): Glyph = this(source)(sheet)
  implicit def XMLtoGlyph(source: Elem)(implicit sheet: Sheet): Glyph = this(source)(sheet)

}

/**
 * A simple translation that can be imported into a context where glyphXML text (etc) will appear.
 * Meanings can be extended in the usual by assigning to `translation`.
 */
object Language {
  implicit val translation: Translation = new Translation {
      import Translation.Target._

      def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(primitives) {
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tag :: tags, paragraph, attributes.updated("textStyle", textStyle), sheet, children)
        }
      }

      meaning("i") = textStyleTranslation("i", "Italic")
      meaning("b") = textStyleTranslation("b", "Bold")
      meaning("bi") = textStyleTranslation("bi", "BoldItalic")
      meaning("n") = textStyleTranslation("n", "Normal")
      meaning("tt") = new Translation(primitives) {
        override def toString: String = "tt"
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tags, paragraph, attributes.updated("textFontFamily", "Courier"), sheet, children)
        }
      }
      meaning("caption") = new Abstraction(<p align="center"><b>&BODY;</b></p>)
  }

  implicit def XMLtoGlyph(source: Node)(implicit sheet: Sheet): Glyph = translation.XMLNodetoGlyph(source: Node)
}



