package org.sufrin


package glyph
package glyphXML

import io.github.humbleui.skija.Font
import org.sufrin.glyph.unstyled.static
import org.sufrin.glyph.unstyled.static.BreakableGlyph
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.glyphXML.Translation.{isBlank, AttributeMap}
import org.sufrin.logging.Default

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._



/**
 * Implementation of simple "hygienic" macro abstractions.
 *
 * When a macro named "name" whose body is `body` is invoked by
 * {{{
 *  <name invocationAttributes > ... </name>
 * }}}
 *
 * its `body` is expanded, then the expansion is
 * treated as if it had been written explicitly.
 *
 * Expansion first filters the invocation body (...), keeping only element-like parts, and visible text.
 * Then certain substitutions are made:
 *
 * {{{
 *   &BODY;         by the entire (filtered) ...
 *   &BODY1;        by the first element of the (filtered) ...
 *   &BODY2;        by the first element of the (filtered) ...
 *   &BODYn;        by the nth element of the (filtered) ...
 *   &BODYn..;      by the elements of the (filtered) ...
 *                  that succeed n, inclusive of `n`.
 *                  (`&BODY;` is equivalent to `&BODY..0;`)
 *   &invAttrName;  by the value of the named invocation attribute
 * }}}
 *
 * Substitutions are also made to attributes of the elements in `body` whose values take one of the forms
 * {{{
 *  "\$invAttrName"           by the value of the invocation attribute
 *                           (defaulting to "")
 *  "\$invAttrName(default)"  as above, defaulting to default
 * }}}
 *
 * Examples are garbled by  ScalaDoc, but are present as comments in the source text of `glyphXML`
 *
 */

class Macro(body: Node) {
/**
 *{{{
 *   <MACRO key="cj">
 *     <div width="\$width(21em)">
 *         <p align="center"><b>CJ in &width;</b></p>
 *         <p align="center">&BODY0;</p>
 *         <p align="right">&BODY1;</p>
 *     </div>
 *   </MACRO>
 *}}}
 *
 * the invocation
 *{{{
 *   <cj width="42"><b>Able</b><span>Baker</span></cj>
 *
 * should expand to
 *
 *   <div width="42em">
 *         <p align="center"><b>CJ in 42em</b></p>
 *         <p align="center"><b>Able</b></p>
 *         <p align="right"><span>Baker</span></p>
 *   </div>
 *}}}
 *
 * and the invocation
 *{{{
 *    <cj><b>Able</b><span>Baker</span></cj>
 *}}}
 * should expand to
 *{{{
 *    <div width="21em">
 *          <p align="center"><b>CJ in 42em</b></p>
 *          <p align="center"><b>Able</b></p>
 *          <p align="right"><span>Baker</span></p>
 *    </div>
 *}}}
 *
 *
 * Finally, with
 * {{{
 * <MACRO key="cj">
 *   <div width="\$width">
 *   <p after="0">&BODY0..;</p>
 *   <p after="1">&BODY1..;</p>
 *   <p after="2">&BODY2..;</p>
 *   <p after="3">&BODY3..;</p>
 *   </div>
 * </MACRO>
 * }}}
 * the invocation
 * {{{
 *  <cj width="42"><b>0</b><b>1</b><b>2</b><b>3</b><b>4</b></cj>
 * }}}
 * should expand to
 * {{{
 *       <div width="42em">
 *          <p after="0"><b>0</b><b>1</b><b>2</b><b>3</b><b>4</b></p>
 *          <p after="1"><b>1</b><b>2</b><b>3</b><b>4</b></p>
 *          <p after="2"><b>2</b><b>3</b><b>4</b></p>
 *          <p after="3"><b>3</b><b>4</b></p>
 *       </div>
 * }}}
 *
 * @param body -- the entire invocation of <MACRO ...>  ...</MACRO>
 */

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
    for { i <- 0 to children.length }
        bindings.addAll(List(s"BODY$i.."->children.drop(i)))
    bindings.addAll(List("BODY"->children))
    // TODO:  ListMap appears to have no effective constructor from lists of pairs -- at least none that IntelliJ Scala accepts

    def substitute(node: Node): Seq[Node] = {
      node match {
        case EntityRef(id) =>
          if (!bindings.contains(id)) Default.warn(s"Macro reference $invocation has no $id in $body")
          bindings.get(id) match {
            case None        => Text(invocationAttributes.getOrElse(id, if (id.startsWith("BODY")) "" else node.toString))
            case Some(nodes) => nodes.flatMap(substitute(_))
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
              actualAttributes.get(ref.toLowerCase) match {
                case Some(value) => value
                case None        => value
              }
            case s"$$$ref" =>
              actualAttributes.get(ref.toLowerCase) match {
                case Some(value) => value
                case None =>
                  Default.warn(s"Macro reference $invocation has no parameter $ref")
                  ""
              }
            case _ => value
          }
        }
        result
      }

      var attribs: MetaData = Null
      for { attr <- attrs } {
        attribs = MetaData.concatenate(attribs, Attribute(null, attr.key.toLowerCase, deref(attr.value.text), attribs))//MetaData.concatenate(attribs, attr.prefixedKey, attr.value.text)
      }

      attribs = MetaData.normalize(attribs, null)
      //println(s"$attrs => $attribs")
      attribs
    }

    val result = substitute(body)

    val logging = invocationAttributes.getOrElse("logging", "")
    if (logging.nonEmpty && logging.toLowerCase != "false") {
      Default.info(s"Expanding $logging  ($body)")
      Default.info(s"  $invocation")
      Default.info(s"  $result")
    }

    result
  }
}


/**
 * Implements (scoped, typed) mappings from  names to their properties used
 * during translation.
 *  {{{<tag ...>}}} for an otherwise-unspecified `tag`, tries the translation scheme `extensionMap(tag)` if there is one;
 *  otherwise invokes the macro `macroMap(tag)` if there is one; otherwise treats the entire element as quoted data.
 *
 * The inherited attributes of an element with tag `label` are the catenation of the
 * globally-declared attributes for `tag:label`, then those of its declared "class",
 * then those of its specific "id".
 * {{{<glyph gid="label" .../>}}} translates with  `styledMap("label")`, if one is defined;
 * otherwise translates to the element `elementMap("label")` if one exists.
 * {{{&name;}}} translates to the literal string `entityMap(name)`
 */
class Primitives {
  val extensionMap:         mutable.Map[String, Translation]  = mutable.LinkedHashMap[String, Translation]()
  val macroMap:             mutable.Map[String, Macro]        = mutable.LinkedHashMap[String, Macro]()
  val globalAttributesMap:  mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val entityMap:            mutable.Map[String, String]       = mutable.LinkedHashMap[String, String]()
  val elementMap:           mutable.Map[String, Elem]              = mutable.LinkedHashMap[String, Elem]()
  val styledMap:            mutable.Map[String, StyleSheet=>Glyph] = mutable.LinkedHashMap[String, StyleSheet=>Glyph]()

  private def restore[K,V](saved: (mutable.Map[K,V], Seq[(K,V)])): Unit = {
    val (original, copy) = saved
    original.clear()
    original.addAll(copy)
  }

  private def save[K,V](map: mutable.Map[K,V]): (mutable.Map[K,V], Seq[(K,V)]) = (map, map.toSeq)

  def scoped[V](body: =>V) = {
    val (m1, m2, m3, m4, m5, m6) = (
      save(extensionMap),
      save(macroMap),
      save(globalAttributesMap),
      save(entityMap),
      save(elementMap),
      save(styledMap)
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
  def update(id: String, glyph: StyleSheet=>Glyph): Unit = styledMap(id.toLowerCase)=glyph
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = globalAttributesMap(id.toLowerCase)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: Translation) = extensionMap(id.toLowerCase) = generator
  /** Declare a new text entity */
  def update(id: String, expansion: String) = entityMap(id.toLowerCase) = expansion
  /** Declare a new expandable entity */
  def update(id: String, node: Node) : Unit = {
    node  match {
      case element: Elem =>
        if (element.label.toLowerCase == "attributes")
          globalAttributesMap(id.toLowerCase) = Translation.normalizeKeys(element.attributes.asAttrMap)
        else
          elementMap(id.toLowerCase) = element

      case other =>
        import Default.error
        error(s"Meaningless XML assignment ...($id) = $node")
    }
  }

  /** Declare a new macro */
  def update(id: String, abbr: Macro) : Unit = macroMap(id.toLowerCase) = abbr
}

object Paragraph {

  def fromGlyphs(sheet: StyleSheet, glyphs: Seq[Glyph], parHang: Option[Glyph]): Glyph = {
    val glyphs$   =
      (if (sheet.parIndent>0) List(static.Rect(sheet.parIndent, 1f, fg=Brushes.transparent)) else Nil) ++ glyphs

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

    val column = NaturalSize.Col(bg = sheet.textBackgroundBrush, align=Left)(galley.toSeq)


    hangGlyph match {
      case None =>
        if (true || leftMargin > 0f)
          NaturalSize.Row(Mid, bg = sheet.textBackgroundBrush)(
              FixedSize.Space(w =  leftMargin, h = 0f, stretch = 0f),
              column,
              FixedSize.Space(w = sheet.rightMargin, h = 0f, stretch = 0f))
        else
          column

      case Some(theGlyph) =>
        val space = FixedSize.Space(leftMargin-theGlyph.w,theGlyph.h, 0f)
        NaturalSize.Row(Mid, bg = sheet.textBackgroundBrush)(
            NaturalSize.Row(Top, bg = sheet.textBackgroundBrush)(theGlyph, space, column),
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
            words.setElement(new BreakableGlyph(breakable.hyphen, glyphs.drop(breakPoint)))
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
            galley += NaturalSize.Row(Top)(glyphs.take(breakPoint)).framed(fg = Brushes.red(width=2))
          case other =>
            galley += NaturalSize.Row(static.FilledRect(maxWidthfloor, other.h, fg = Brushes.red(width=2)))
        }
        words.nextElement()
      } else {
        val (width, glyphs) = setLine()
        // the line had only its starting alignment glyph; transparent else to do
        if (glyphs.length == 1 && width == 0) {
          setting = false
        } else
          galley += FixedSize.Row(maxWidth, align=Baseline)(glyphs)
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
        val source = unstyled.Text(text, font, fg, bg)
        source
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

      import unstyled.{Text => makeText}
      val asGlyph = if (text.contains(discretionaryWordBreak)){
        val glyphs = text.toString.split(discretionaryWordBreak).toSeq.map { syllable =>makeText(syllable, font, fg, bg) }
        val hyphen = makeText("-", font, fg, bg)
        new BreakableGlyph(hyphen, glyphs)
      }
      else makeText(text, font, fg, bg)

    }

    /** The structure of a paragraph to be made from `targets` */
    case class ParaTarget(sheet: StyleSheet, targets: Seq[Target], parHang: Option[Glyph]) extends Target with PrettyPrint.PrettyPrintable
    { val arity=3
      val prefix="Para"
      override def field(i: Int): (String, Any) = i match {
        case 2 => ("chunks", targets)
        case 1 => ("", s"w: ${sheet.parWidth}")
        case 0 => ("hang", parHang)
      }
      val asGlyph: Glyph = Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)
    }

    case class ColTarget(background: Brush, chunks: Seq[Target], alignment: Alignment = Left, frame: Brush=Brushes.transparent) extends Target {
      val theGlyphs = chunks.map(_.asGlyph)
      val theGlyph =  NaturalSize.Col(align=alignment, bg=background, frame=frame)(theGlyphs)
      val asGlyph: Glyph = theGlyph
    }

    case class GlyphTarget(paragraph: Boolean, sheet: StyleSheet, glyph: Glyph) extends Target with PrettyPrint.PrettyPrintable {
      val arity=2
      val prefix="GlyphTarget"
      override def field(i: Int): (String, Any) = i match {
        case 1 => ("glyph", glyph)
        case 0 => ("para",  paragraph)
      }

      val glyph$ = glyph
      val asGlyph: Glyph = glyph$
    }

    case class DecorateTarget(tags: Seq[String], paragraph: Boolean, attributes: TypedAttributeMap, target: Target, ambientFont: Font, ambientSheet: StyleSheet) extends Target {
      @inline def raiseBy(glyph: Glyph, by: Scalar): Glyph = glyph.withBaseline(by)

      def decorated(glyph: Glyph): Glyph = {
        val g0 = glyph
        def rotate(glyph: Glyph): Glyph = {
          val rotated = attributes.Int("rotated", 0)
          if (rotated==0) glyph else
          if (!paragraph)
            glyph.rotated(rotated) else
          rotated % 4 match {
            case 0 => glyph
            case _ => glyph.rotated(rotated)
          }
        }

        def turn(glyph: Glyph): Glyph =
          attributes.Int("turned", 0) match {
            case 0 => glyph
            case d =>
              val glyph$ = glyph.turned(d.toFloat, false)
              glyph$
          }

        def frame(glyph: Glyph): Glyph = {
          val brush = attributes.Brush("frame", Brushes.invisible)
          val framing = attributes.Bool("framed", brush.getAlpha != 0)
          val rad: Scalar = attributes.Float("radius", 0)
          if (framing)
            glyph.roundFramed(radius=rad, fg = brush, bg = attributes.Brush("bg", glyph.bg))
          else
            glyph
        }

        def edge(glyph: Glyph): Glyph = {
          val brush = attributes.Brush("edge", Brushes.invisible)
          val framing = attributes.Bool("edged", brush.getAlpha != 0)
          if (framing)
            glyph.edged(fg = brush, bg = attributes.Brush("bg", glyph.bg))
          else
            glyph
        }

        def scale(glyph: Glyph): Glyph = {
          val scale: Scalar = attributes.Float("scaled", 1f)
          if (scale!=1f) glyph.scaled(scale) else glyph
        }

        def enlarge(glyph: Glyph): Glyph = {
          val points: Scalar = attributes.Units("enlarged", 0f)(ambientSheet)
          if (points!=0f) glyph.enlarged(points) else glyph
        }

        // compile the decor program
        val decor = attributes.String("decorate","rotate;edge;turn;scale;enlarge;frame").split(';').toSeq
        var transformed = glyph
        for { transform <- decor } {
          transform match {
            case "rotate"  => transformed = rotate(transformed)
            case "edge"    => transformed = edge(transformed)
            case "turn"    => transformed = turn(transformed)
            case "scale"   => transformed = scale(transformed)
            case "enlarge" => transformed = enlarge(transformed)
            case "frame"   => transformed = frame(transformed)
            case _ =>
              logging.Default.warn(s"Bad decorate transformation: ($transform) in $attributes")
          }
        }
        transformed
      }

      import ambientFont.getMetrics
      val ambientH = getMetrics.getHeight
      val glyph$ = decorated(target.asGlyph)
      val asGlyph: Glyph = raiseBy(glyph$, 0.5f*(glyph$.h+ambientH))

    }

  }

  def isBlank(elem: Node): Boolean = elem match {
    case Text(data) => data.isBlank
    case _ => false
  }
}

/**
 *  Provides methods for interpreting the value parts of the `key="value"` pairs of the given `AttributeMap`
 *  as (Scala-)typed values. Each accessor method supplies a `key` and an alternate value: its result
 *  is the string value of `key` in the map (if there is one) interpreted as a (scala) value of the
 *  appropriate type; or the alternative value if `key` is not in the map.
 */
class TypedAttributeMap(unNormalized: AttributeMap) {
  import logging.Default.warn

  /** Show as a mapping with pairs in the form `k->d` */
  override def toString: String = Visitor.toString(attributes)
  /** Show as a mapping with pairs in the form `k->d` */
  def asString: String = Visitor.toString(attributes)

  /**
   *  TODO: The attribute mapping (used to be "normalized", and may one day be so again)
   */
  val attributes: AttributeMap = unNormalized // .map{ case (k,d) => (k.toLowerCase, d)}

  val at=attributes.getOrElse("source", "")


  /**
   * Yields the string with key `key`, or `alt`
   */
  def String(key: String, alt: String): String = attributes.getOrElse(key.toLowerCase, alt)

  /**
   * Yields the Int with key `key` (if it looks like a number), or `alt`
   */
  def Int(key: String, alt: Int): Int = attributes.get(key.toLowerCase) match {
    case Some (s) if s.matches("-?[0-9]+") => s.toInt
    case Some(s)  =>
      warn(s"$key(=$s) should be an Int [using $alt] ($at)")
      alt
    case None     => alt
  }

  /**
   * Yields Float Int with key `key` (if it looks like a floating point number), or `alt`
   */
  def Float(key: String, alt: Float): Float = attributes.get(key.toLowerCase) match {
    case Some (spec) =>
      try {
        spec.toFloat
      }
      catch {
        case exn: Throwable  => Default.warn(s"$key(=$spec) should be a Float [using $alt] ($at)")
          alt
      }
    case None     => alt
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
  def Units(key: String, alt: Float)(sheet: StyleSheet): Float = {
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
              case "width" => factor*sheet.parWidth
              case "indent" => factor*sheet.parIndent
              case "leftmargin" => factor*sheet.leftMargin
              case "rightmargin" => factor*sheet.rightMargin
              case "container.width" => factor*sheet.containerWidth
              case "container.height" => factor*sheet.containerHeight
              case "window.width" => factor*sheet.windowWidth
              case "window.height" => factor*sheet.windowHeight
              case "screen.width" => factor*sheet.screenWidth
              case "screen.height" => factor*sheet.screenHeight
              case other =>
                warn(s"$key(=$other) should specify its unit of measure in em/ex/px/pt, or as a <float>*(width/indent/leftmargin/rightmargin/windowwidth/windowheight). ($at)" )
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
  def Bool(key: String, default: Boolean): Boolean = attributes.get(key.toLowerCase) match {
    case None => default
    case Some(boolean) =>
      boolean.toLowerCase match {
        case "t" | "true"  | "on" => true
        case "f" | "false" | "off" => false
        case _ =>
          warn(s"$key=\"$boolean\" should be t/f/true/on/false/off ($at)")
          default
      }
  }

  /**
   * Yields the lateral alignment with key `key` (if it's one of `{left, right, center, justify}`), or `default`
   */
  def Align(key: String, default: Alignment): Alignment = attributes.get(key.toLowerCase) match {
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
  def VAlign(key: String, default: VAlignment): VAlignment = attributes.get(key.toLowerCase) match {
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
   *  Yields the brush whose `Brushes` specification has key `key`, or `alt`.
   *
   * @see Brushes
   */
  def Brush(key: String, alt: Brush): Brush = attributes.get(key.toLowerCase) match {
    case None       => alt
    case Some(name) => Brushes(name)
  }

  /**
   * The `StyleSheet` derived from `sheet` by associating new values with
   * the properties  specified  by the attribute mapping.
   *
   */
  def declareAttributes(sheet: StyleSheet): StyleSheet = {
    val fontDetail: StyleSheet =
      sheet
        .copy(
          fontScale       = Float("fontScale", 1.0f),

          textFontStyle   = FontFamily.styleNamed(String("textStyle", "")),
          textFontFamily  = FontFamily(String("fontFamily", String("textFontFamily", sheet.textFontFamily.family))),
          textFontSize    = Float("fontSize", Float("textFontSize", sheet.textFontSize)),

          labelFontStyle  = FontFamily.styleNamed(String("labelStyle", "")),
          labelFontFamily = FontFamily(String("fontFamily", String("labelFontFamily", sheet.labelFontFamily.family))),
          labelFontSize   = Float("fontSize", Float("labelFontSize", sheet.labelFontSize)),

          buttonFontStyle  = FontFamily.styleNamed(String("buttonStyle", "")),
          buttonFontFamily = FontFamily(String("fontFamily", String("buttonFontFamily", sheet.buttonFontFamily.family))),
          buttonFontSize   = Float("fontSize", Float("buttonFontSize", sheet.buttonFontSize)),
        )

    // Units are computed relative to the font details, which may have been redeclared
    fontDetail.copy(
      padX                  = Units("padX",           sheet.padX)         (fontDetail),
      padY                  = Units("padY",           sheet.padY)         (fontDetail),
      parWidth              = Units("width",          sheet.parWidth)     (fontDetail),
      parSkip               = Units("parSkip",        sheet.parSkip)      (fontDetail),
      parIndent             = Units("parIndent",      sheet.parIndent)    (fontDetail),
      leftMargin            = Units("leftMargin",     sheet.leftMargin)   (fontDetail),
      rightMargin           = Units("rightMargin",    sheet.rightMargin)  (fontDetail),
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

/**
 * Implementation details for translation methods.
 * @param primitives
 */
class Translation(val primitives: Primitives=new Primitives) {
  import Translation.Target._
  import Visitor.AttributeMap
  import primitives._
  implicit class TypedMap(attributes: AttributeMap) extends TypedAttributeMap(attributes)
  val meaning: Translation = this

  import primitives.entityMap
  /** Declare a named glyph generator */
  def update(id: String, glyph: StyleSheet=>Glyph): Unit = primitives(id)=glyph
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
  def update(id: String, abbr: Macro) : Unit = primitives(id) = abbr

  protected
  def extendContextFor(tag: String)(inherited: AttributeMap, local: Map[String,String]): AttributeMap = inherited ++ local

  protected
  def translateText(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, text: String): Seq[Target] = {

    @inline def solidText(text: String): Target = SolidTextTarget(text, paragraph, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush)
    @inline def hyphenatableText(text: String): Target = HyphenatableTextTarget(text, sheet.discretionaryWordBreak, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush)

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
      // List(WordGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).Left(lines), sheet.textFontStyle.toString))
      List(ColTarget(sheet.backgroundBrush, lines ))
    }
  }

  /**
   * Translates the children of {{{<tag attributes>child1 ...</tag>}}} to the corresponding sequence of `Translation` elements, taking
   * into account any stylesheet declarations made in `attributes`.
   * @param tags list of currently-in-scope tags
   * @param paragraph true if within a <p ...>...</p> tag
   * @param attributes attribute mapping for the given tag
   * @param sheet currently-effective style sheet
   * @param children nodes to be translated
   * @return sequence of `Target` translations for further processing
   */
  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, children: Seq[Node]): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    children.flatMap { source => translate(tags, paragraph, attributes, sheet$, source) }
  }

  /**
   * By dispatching on the kind of `source`, this translates the given `source: Node` to the corresponding sequence of `Translation` elements.
   * This is the main workhorse for the basic translation of `<tag ...>...</tag>` structures represented as `xml.Elem` nodes.
   */
  def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, source: Node): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    def tagString:String = tags.reverse.mkString("<", "<", "")

    source match {
      case xml.EntityRef(name) => translateText(tags, paragraph, attributes, sheet$, entityMap.getOrElse(name, s"&$name;"))

      case xml.Text(text) =>
        translateText(tags, paragraph, attributes, sheet, text)

      case xml.PCData(text: String) =>
        translatePCData(tags, paragraph, attributes, sheet$, text)

      case xml.ProcInstr(target, text) =>
        translateProcInstr(tags, target, text)

      case xml.Comment(text) => Seq.empty

      case xml.Elem(str, tag, metaData, binding, children@_*) =>

        val localAttributes = Translation.normalizeKeys(metaData.asAttrMap)

        def attrsFor(attrId: String): AttributeMap = {
          localAttributes.get(attrId) match {
            case None       => Map.empty
            case Some(attr) =>
              globalAttributesMap.get(attr) match {
                case None =>
                  import logging.Default.warn
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
        val specifiedAttributes: AttributeMap = globalAttributesMap.getOrElse(s"tag:${tag}", Map.empty) ++ attrsFor("class") ++ attrsFor("id")

        /**
         * The effective attributes of an element are the catenation of its default attributes and its actually appearing
         * attributes, without "id" and "class"
         */
        val attributes$$ : AttributeMap = specifiedAttributes ++ localAttributes.filterNot{ case (key, _) => key.toLowerCase=="class" || key.toLowerCase=="id"}
        val attributes$ : AttributeMap = specifiedAttributes ++ localAttributes
        val sheet$ = attributes$.declareAttributes(sheet)
        val tags$ = tag::tags

        def Decorated(t: Target): Seq[Target] = List(DecorateTarget(tags$, paragraph, attributes$, t, sheet$.textFont, sheet$))

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
              else
                Some(styled.Label(hangString)(sheet$))
            val chunks = children.flatMap { source => translate(tags$, true, attributes$$, sheet$, source) }
            Decorated(ParaTarget(sheet$, chunks, hang))

          case "div" | "body" =>
            val children$ = children.filterNot(Translation.isBlank(_))

            Decorated(ColTarget(sheet.backgroundBrush, chunks = translate(tags$, false, attributes$, sheet$, children$)))

          case "col" =>
            val glyphs: Seq[Target] = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, paragraph, attributes$$, sheet$, source) }
            val alignment = attributes$.Align("align", Left)
            val frame     = attributes$.Brush("sep", attributes$.Brush("separator", Brushes.transparent))
            val debug= attributes$.Bool("debug", false)
            if (debug) println(glyphs)
            Decorated(ColTarget(sheet$.backgroundBrush, glyphs, alignment, frame))


          case "row" =>
            val inheritWidth = attributes$.Bool("inheritwidth", false)
            val uniform = attributes$.Bool("uniform", false)
            val defaultWidth = if (inheritWidth) sheet$.parWidth else 0f
            val width = attributes$.Units("width", defaultWidth)(sheet$)
            val glyphs = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, false, attributes$$, sheet$, source) }.map(_.asGlyph)
            val valign: VAlignment = attributes$.VAlign("valign", Top)
            val glyph = if (width==0f)
                 NaturalSize.Row(align=valign, uniform=uniform, bg=sheet$.backgroundBrush)(glyphs)
            else
                 FixedSize.Row(align=valign, width=width, bg=sheet$.backgroundBrush)(glyphs)
            Decorated(GlyphTarget(paragraph, sheet$, glyph))

          case "fill" =>
            val width  = attributes$.Units("width", sheet$.emWidth)(sheet$)
            val height = attributes$.Units("height", sheet$.exHeight)(sheet$)
            val stretch = attributes$.Float("stretch", 1f)
            val background = attributes$.Brush("background", attributes$.Brush("bg", Brushes.transparent))
            val foreground = attributes$.Brush("foreground", attributes$.Brush("fg", Brushes.transparent))
            //** DO NOT DECORATE: IT MESSES UP FIXEDSIZE STRETCHING **//
            List((GlyphTarget(paragraph, sheet$, FixedSize.Space(width, height, stretch, fg=foreground, bg=background))))

          case "glyph" =>
            //println (s"<glyph ${attributes$.asString}/>")
            val id: String = localAttributes.String("gid", alt="")
            if (styledMap.get(id.toLowerCase).isDefined)
               Decorated(GlyphTarget(paragraph, sheet$, generateGlyph(tags$, paragraph, attributes$, sheet$, id.toLowerCase)))
            else {
               elementMap.get(id.toLowerCase) match {
                 case None =>
                   logging.Default.warn (s"$tagString <glyph ${attributes.asString}/> UNDEFINED (no generator or element)")
                   Decorated(GlyphTarget(paragraph, sheet$, styled.Label (s"UNDEFINED $id")(sheet$.copy(labelForegroundBrush = Brushes.red))))
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
              glyph = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY, width=width).rows(glyphs)))

          case "cols" =>
            import sheet$.{padX, padY, backgroundBrush => bg, foregroundBrush => fg}
            val width  = attributes$.Int("columns", attributes$.Int("cols", 0))
            val height = attributes$.Int("rows", 0)
            val glyphs = children.filterNot(isBlank(_)).flatMap { source => translate(tags$, paragraph, attributes$, sheet$, source) }.map(_.asGlyph)
            //println(s"$padX, $padY")
            Decorated(GlyphTarget(paragraph = paragraph,
              sheet = sheet$,
              glyph = NaturalSize.Grid(fg = fg, bg = bg, padx=padX, pady = padY, height=height).cols(glyphs)))

          case "span" =>
            children.flatMap { child => translate(tags$, paragraph, attributes$, sheet$, child) }

          case "SCOPE" =>
            primitives.scoped(children.flatMap { child => translate(tags$, paragraph, attributes$, sheet$, child) })

          case "ATTRIBUTES" =>
            // Set the attributes defined for the given id.
            // The scope of this is the body of the closest enclosing nonempty <ATTRIBUTES></ATTRIBUTES>; but is global if the body is empty.
            val id = localAttributes.String("key", "")
            val attrs = attributes$$.removedAll(List("key"))
            globalAttributesMap(id)=attrs
            val attributes$$$=attributes.removedAll(List("key", "class"))
            val result = children.flatMap { child => translate(tags$, paragraph, attributes$$$, sheet$, child) }
            result

          case "MACRO" =>
              // Define the macro named after the given key: the effect is to introduce a new kind of tagged element
              // The scope of this is the body of the closest enclosing <SCOPE></SCOPE> element; but is global if there is no such element.
              val id = localAttributes.String("key", "")
              macroMap(id) = new Macro(children.head)
              List()

          case "ENTITY" =>
            // Set the attributes defined for the given key.
            // The scope of this is the body of the closest enclosing nonempty <ATTRIBUTES></ATTRIBUTES>; but is global if the body is empty.
            val id = localAttributes.String("key", "")
            val expansion = localAttributes.String("expansion", "")
            entityMap(id) = expansion
            val attributes$$$=attributes$.removedAll(List("id", "expansion"))
            val result = children.flatMap { child => translate(tags$, paragraph, attributes$$$, sheet$, child) }
            result

          case "ELEMENT" =>
            // Define the element corresponding to the given key.
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
            import logging.Default.fine
            val caption=attributes$.String("AT", "")
            fine(s"ATTRIBUTES($caption) ${tagString}<attributes ${attributes$.asString} ${if (paragraph) "(P)" else ""}")
            Seq.empty


          case _ =>
            extensionMap.get(tag) match {
              case Some(translation) =>
                //println(s"<$tag special translation ${Visitor.toString(attributes$)}")
                translation.translate(tags$, paragraph, attributes$, sheet$, children)

              case None =>
                macroMap.get(tag) match {
                  case None =>
                    translatePCData(tags$, false, Map.empty, StyleSheet(), source.toString())
                  case Some(abstraction) =>
                    //println(s"Macro ${attributes$.asString}")
                    val expanded: Seq[Node] = abstraction.expansion(attributes$, source)
                    translate(tags$, paragraph, attributes$, sheet$, expanded)
                }
            }

        }


      case _ =>
        // private representation as a scala.xml.Atom
        translateText(tags, paragraph, attributes, sheet, s" $source ")

    }

  }

  /** Generate a glyph from the given glyph `ref`erence  */
  private def generateGlyph(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, ref: String): Glyph =
    styledMap.get(ref) match {
      case None =>
            logging.Default.warn (s"<glyph ${attributes.asString}/> UNDEFINED (no generator)")
            styled.Label (s"UNDEFINED $ref") (sheet.copy (labelForegroundBrush = Brushes.red) )
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

  protected def stripCommonIndentation(lines: Seq[String]): Seq[String] = {
    val lines$ = lines.dropWhile(_.isBlank)
    val prefix = leadingSpaces(lines$.filterNot(_.isBlank).min(indentationOrdering))
    def stripPrefix(line: String): String = if (line.length<prefix) line else line.substring(prefix)
    if (lines$.nonEmpty && lines$.last.isBlank)
      lines$.init.map(stripPrefix)
    else
      lines$.map(stripPrefix)
  }

  /**
   * Translate {{{<[CDATA[ ... ]]>}}} yields the literal text ... as a left-aligned sequence of lines.
   * When the translation is done in a context where `normalizePCData="true"` the lines
   * are "normalized", by shifting them to the left as a whole until their leftmost non-space is at the left margin.
   *
   */
  def translatePCData(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, text: String): Seq[Target] = {
    import sheet.{textFont, backgroundBrush => colBG, textBackgroundBrush => bg, textForegroundBrush => fg}
    import unstyled.{Text => makeText}
    val textBG = if (bg.alpha == 0f) colBG else bg
    val rawLines = text.split('\n').toSeq
    val lines    = if (attributes.Bool("normalizePCData", true)) stripCommonIndentation(rawLines) else rawLines
    val glyphs   = lines.map(makeText(_, textFont, fg, textBG))
    List(GlyphTarget(paragraph, sheet, NaturalSize.Col(align=Left, bg = colBG)(glyphs)))
  }


  /** Translate a processing instruction `<?target text ?>` does transparent*/
  def translateProcInstr(tags: List[String], target: String, text: String): Seq[Target] = Seq.empty

  /**
   *  For `translator: Translation`, `source: xml.Node` , and `sheet: StyleSheet`, the Scala computation `trans(source)(sheet)`
   *  yields the glyph that corresponds to the translated source. This glyph is composed of a centre-aligned `NaturalSize.Col`
   *  of the Glyph-representations of the `Target`s generated by:
   *  {{{translator.translate(Nil, false, Map.empty, sheet, source)}}}
   */
  def apply(source: Node)(implicit sheet: StyleSheet): Glyph = {
    val translator = this
    //import PrettyPrint.AnyPretty
    //for { t <- (translator.translate(Nil, false, Map.empty, sheet, source)) } t.prettyPrint()
    NaturalSize.Col(align=Center)(translator.translate(Nil, false, Map.empty, sheet, source).map(_.asGlyph))
  }

  /** Implicit coercion from an `xml.Node` to the `Glyph` it represents with the given `StyleSheet` */
  implicit def XMLNodetoGlyph(source: Node)(implicit sheet: StyleSheet): Glyph = this(source)(sheet)
  /** Implicit coercion from an `xml.Elem` to the `Glyph` it represents with the given `StyleSheet` */
  implicit def XMLtoGlyph(source: Elem)(implicit sheet: StyleSheet): Glyph = this(source)(sheet)

}

/**
 * A simple `Translation` that can be imported into a context where glyphXML source will appear.
 * Meanings can be extended in the usual by assigning to `translation`.
 */
object Language {
  implicit val translation: Translation = new Translation {
      import Translation.Target._

      def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation(primitives) {
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tag :: tags, paragraph, attributes.updated("textStyle", textStyle), sheet, children)
        }
      }

      meaning("i") = textStyleTranslation("i", "Italic")
      meaning("b") = textStyleTranslation("b", "Bold")
      meaning("bi") = textStyleTranslation("bi", "BoldItalic")
      meaning("n") = textStyleTranslation("n", "Normal")
      meaning("tt") = new Translation(primitives) {
        override def toString: String = "tt"
        override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: StyleSheet, children: Seq[Node]): Seq[Target] = {
          super.translate(tags, paragraph, attributes.updated("textFontFamily", "Courier"), sheet, children)
        }
      }
      meaning("center")  = new Macro(<row width="1*width"><fill/>&BODY;<fill/></row>)
      meaning("caption") = new Macro(<p align="center"><b>&BODY;</b></p>)
  }

  /** Implicit coercion from an `xml.Node` to the `Glyph` it represents with the given `StyleSheet`*/
  implicit def XMLtoGlyph(source: Node)(implicit sheet: StyleSheet): Glyph = translation.XMLNodetoGlyph(source: Node)
}



