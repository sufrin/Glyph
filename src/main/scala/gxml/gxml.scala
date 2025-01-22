package org.sufrin.glyph
package gxml


import io.github.humbleui.skija.Font
import org.sufrin.glyph.gxml.Visitor.AttributeMap
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.logging.Default

import scala.annotation.nowarn
import scala.collection.mutable
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

trait Primitives {
  val translationMap:   mutable.Map[String, Translation]  = mutable.LinkedHashMap[String, Translation]()
  val abstractionMap:   mutable.Map[String, Abstraction]  = mutable.LinkedHashMap[String, Abstraction]()
  val attrMap:          mutable.Map[String, AttributeMap] = mutable.LinkedHashMap[String, AttributeMap]()
  val entityMap:        mutable.Map[String, String]       = mutable.LinkedHashMap[String, String]()
  val elementMap:       mutable.Map[String, Elem]         = mutable.LinkedHashMap[String, Elem]()
  val glyphMap:         mutable.Map[String, Glyph]         = mutable.LinkedHashMap[String, Glyph]()


  /** Declare a named glyph */
  def update(id: String, glyph: Glyph): Unit      = glyphMap(id)=glyph
  /** Declare a named attribute map: used for inheritance of attributes */
  def update(id: String, map: AttributeMap): Unit = attrMap(id)=map
  /** Declare a new kind of tag */
  def update(id: String, generator: Translation) = translationMap(id) = generator
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
}

object Translation {

  object Target {
    trait Target
    /** Joins its predecessor to its successor */
    case object JoinTarget extends Target with PrettyPrint.PrettyPrintable {
      /** The name of the class (or object) */
      def prefix: String = "Join"

      /** The number of fields/elements of the object */
      def arity: Int = 0
    }

    case class TextTarget(text: String, atBase: Boolean, font: Font, fg: Brush, bg: Brush) extends Target with PrettyPrint.PrettyPrintable {
      val arity = 5
      val prefix = "Text"
      override def field(i: Int): (String, Any) = i match {
        case 1=>("atBase", atBase)
        case 0=>("", s""""$text"""")
        case 2=>("font", FontFamily.fontString(font))
        case 3=>("fg", fg)
        case 4=>("bg", bg)
      }
    }

    case class ParaTarget(width: Scalar, leftMargin: Scalar, rightMargin: Scalar, chunks: Seq[Target]) extends Target

    case class ColTarget(chunks: Seq[Target]) extends Target

    case class GlyphTarget(glyph: Glyph) extends Target
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

  def Align(key: String, default: Alignment)(implicit source: SourceLocation): Alignment = attributes.get(key) match {
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

class Translation extends Primitives {
  import Visitor.AttributeMap
  import Translation.Target._

  implicit class TypedMap(attributes: AttributeMap) extends TypedAttributeMap(attributes)

  def extendContextFor(tag: String)(inherited: AttributeMap, local: Map[String,String]): AttributeMap = {
    inherited ++ local
  }


  def translateText(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {

    @inline def wordGlyph(text: String): Target = TextTarget(text, paragraph, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush)

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
      val lines = text.split("[\n]").toSeq.map(wordGlyph(_))
      // List(WordGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).atLeft$(lines), sheet.textFontStyle.toString))
      List(ColTarget(lines))
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

        def attrsFor(attrId: String): AttributeMap = {
          metaData.asAttrMap.get(attrId) match {
            case None       => Map.empty
            case Some(attr) =>
              attrMap.get(attr) match {
                case None =>
                  import org.sufrin.logging.Default.warn
                  warn(s"No defaults for $attrId=\"$attr\"  in ${tags.reverse.mkString("<", "<", "...>")} ")
                  Map.empty

                case Some(attrs) =>
                  org.sufrin.logging.Default.info(s"attrsFor($attrId) $attr = $attrs")
                  attrs
              }
          }
        }

        /**
         * The inherited attributes of an element with tag `label` are the catenation of the
         * globally-declared attributes for `tag:label`, then those of its declared "class",
         * then those of its specific "id".
         */
        val inheritedAttributes: AttributeMap = attrMap.getOrElse(s"tag:${tag}", Map.empty) ++ attrsFor("class") ++ attrsFor("id")

        /**
         * The effective attributes of an element are the catenation of its default attributes and its actually-appearing
         * attributes.
         */
        val attributes$ : AttributeMap = inheritedAttributes ++ metaData.asAttrMap
        val sheet$ = attributes$.declareAttributes(sheet)
        val tags$ = tag::tags
        tag match {
          case "p" =>
            val chunks = child.flatMap { source => translate(tags$, true, attributes$, sheet$, source) }
            // MAKE CHUNKS A PARAGRAPH GLYPH
            import sheet$.{parWidth, leftMargin, rightMargin}
            List(ParaTarget(parWidth, leftMargin, rightMargin, chunks))

          case "glyph" =>
            val id: String     = attributes$.String("id", alt="")
            val copy: Boolean  = attributes$.Bool("copy", !attributes$.Bool("share", true))
            val fg             = attributes$.Brush("fg",   DefaultBrushes.black)
            val bg             = attributes$.Brush("bg",   DefaultBrushes.nothing)
            glyphMap.get(id)  match {
              case None =>
                Seq.empty
              case Some(glyph: Glyph) =>
                List(GlyphTarget(if (copy) glyph(fg, bg) else glyph))
            }



          case _ =>
            translationMap.get(tag) match {
              case Some(translation) =>
                //println(s"<$tag special translation")
                translation.translate(tags$, paragraph, attributes$, sheet$, child)

              case None =>
                //println(s"<$tag default translation")
                child.flatMap { source => translate(tags$, false, attributes$, sheet$, source) }
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

  def translatePCData(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] =
      translateText(tags, false, attributes, sheet, text)

  def translateProcInstr(tags: List[String], target: String, text: String): Seq[Target] = Seq.empty
}


object gxml {
  import xml._
  import Translation.Target._

  def flatten(node: Node): Unit = {
    import Visitor.AttributeMap
    val v = new Visitor {

      def visitText(attributes: AttributeMap, text: String): Unit = println(s"$attributes: \"${text.replaceAll("[\\n][ ]*", "⎆")}\"")

      def visitEntity(attributes: AttributeMap, name: String): Unit = println(s"$attributes: &$name;")

      override def extendContextFor(attributes: AttributeMap, tag: String, attrMap: Map[String,String]): AttributeMap = {
        val map$ : Map[String,String] = attrMap.map{ case (k, d) => (k, s"$tag:$d")}
        attributes ++ map$
      }

      override def visitPCData(attributes: AttributeMap, text: String): Unit = visitText(attributes, s"<![PCDATA[$text]]")

      override def visitProcInstr(attributes: AttributeMap, target: String, text: String): Unit = {}
    }
    v.visit(Map.empty, node)
  }

  def trans(node: Node): Unit = {
    import Visitor.AttributeMap
    val v = new Visitor {

      override def visitText(attributes: AttributeMap, text: String): Unit = print(text)

      override def visitEntity(attributes: AttributeMap, name: String): Unit = print(s"&$name;")

      override def visitPCData(attributes: AttributeMap, text: String): Unit = visitText(attributes, s"<![PCDATA[$text]]")

      override def visitProcInstr(attributes: AttributeMap, target: String, text: String): Unit = {}
    }
    v.visit(Map.empty, node)
  }

  def textStyleTranslation(tag: String, textStyle: String): Translation = new Translation {
    override def translate(tags: List[String], paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] =
      super.translate(tag::tags, paragraph, attributes.updated("textStyle", textStyle), sheet, child)
  }

  val translator: Translation = new Translation

  locally {
    translator("i")  = textStyleTranslation("i", "Italic")
    translator("b")  = textStyleTranslation("b", "Bold")
    translator("bi") = textStyleTranslation("bi", "BoldItalic")
    translator("n")  = textStyleTranslation("n", "Normal")
    translator("today") = "Expansion of today"
    translator("yesterday") = "Expansion of yesterday"
    implicit val sheet = Sheet()
    import sheeted.{Label,TextButton}
    translator("B1")     = Label("B1")
    translator("B2")     = Label("B2") // TextButton("UNSHARED") { _=> }
    translator("B3")     = Label("B3") // TextButton("UNSHARED") { _=> }
    translator("level1")   = <ATTRIBUTES width="500px"/>
    translator("B1")       = <ATTRIBUTES fg="red/60" copy="true"/>
    translator("B2")       = <ATTRIBUTES fg="lightGrey/60" copy="true"/>
    translator("B3")       = <ATTRIBUTES fg="lightGrey/60" share="true"/>
    translator("tag:p")    = <ATTRIBUTES bg="lightGrey/60" width="60px" leftMargin="10px"/>
  }


  def translate(source: Node): Unit = {
    import PrettyPrint.AnyPretty
    for { t <- (translator.translate(Nil, false, Map.empty, Sheet(), source)) } t.prettyPrint()
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

  def main(args: Array[String]): Unit = {
    translate(h)
    println("-----")
  }

}

