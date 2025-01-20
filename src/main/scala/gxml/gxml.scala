package org.sufrin.glyph
package gxml


import io.github.humbleui.skija.Font
import org.sufrin.glyph.gxml.Translation.Target
import org.sufrin.glyph.gxml.Visitor.AttributeMap

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

  /** Declare a named glyph */
  //def update(id: String, glyph: Glyph): Unit       = glyphMap(id)=Literal(glyph)
  //def update(id: String, glyph: () => Glyph): Unit = glyphMap(id)=Generator(glyph)
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

    case object SpaceTarget extends Target with PrettyPrint.PrettyPrintable {
      /** The name of the class (or object) */
      def prefix: String = "Space"

      /** The number of fields/elements of the object */
      def arity: Int = 0
    }

    case class TextTarget(atBase: Boolean, text: String, font: Font, fg: Brush, bg: Brush) extends Target with PrettyPrint.PrettyPrintable {
      val arity = 5
      val prefix = "Text"
      override def field(i: Int): (String, Any) = i match {
        case 0=>("atBase", atBase)
        case 1=>("text", text)
        case 2=>("font", FontFamily.fontString(font))
        case 3=>("fg", fg)
        case 4=>("bg", bg)
      }
    }

    case class ParaTarget(chunks: Seq[Target]) extends Target

    case class ColTarget(chunks: Seq[Target]) extends Target
  }
}

class Translation extends Primitives {
  import Visitor.AttributeMap
  import Translation.Target._

  implicit class TypedAttributeMap(attributes: AttributeMap) {
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

  def extendContextFor(tag: String)(inherited: AttributeMap, local: Map[String,String]): AttributeMap = {
    inherited ++ local
  }


  def translateText(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {

    @inline def wordGlyph(text: String): Target = TextTarget(paragraph, text, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush)

    if (paragraph) {
      val chunks = mutable.ArrayBuffer[Target]()
      def out(t: Target): Unit = chunks += t
      if (text.startsWith(" ") || text.startsWith("\n") || text.startsWith("\t")) out(SpaceTarget)
      val it = text.split("[\t\n ]+").filterNot(_.isBlank).iterator // TODO: precompile the pattern
      while (it.hasNext) {
        // out(WordGlyph(wordAtBaseline(it.next()), sheet.textFontStyle.toString))
        out(wordGlyph(it.next()))
        if (it.hasNext) out(SpaceTarget)
      }
      if (text.endsWith(" ") || text.endsWith("\n") || text.endsWith("\t")) out(SpaceTarget)
      chunks.toSeq
    } else {
      val lines = text.split("[\n]").toSeq.map(wordGlyph(_))
      // List(WordGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).atLeft$(lines), sheet.textFontStyle.toString))
      List(ColTarget(lines))
    }
  }

  def translate(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)
    child.flatMap { source => translate(paragraph, attributes, sheet$, source) }
  }

  def translate(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, source: Node): Seq[Target] = {
    val sheet$ = attributes.declareAttributes(sheet)

    source match {
      case xml.EntityRef(name) => translateText(paragraph, attributes, sheet$, entityMap.getOrElse(name, s"&name;"))

      case xml.Elem(str, tag, localAttrs, binding, child@_*) =>
        val attributes$ : AttributeMap = extendContextFor(tag)(attributes, localAttrs.asAttrMap)
        tag match {
          case "p" =>
            val chunks = child.flatMap { source => translate(true, attributes$, sheet$, source) }
            // MAKE CHUNKS A PARAGRAPH GLYPH
            List(ParaTarget(chunks))

          case _ =>
            translationMap.get(tag) match {
              case Some(translation) =>
                //println(s"<$tag special translation")
                translation.translate(paragraph, attributes$, sheet$, child)

              case None =>
                //println(s"<$tag default translation")
                child.flatMap { source => translate(false, attributes$, sheet$, source) }
            }

        }

      case xml.Text(text) =>
           translateText(paragraph, attributes, sheet, text)

      case xml.PCData(text: String) =>
           translatePCData(paragraph, attributes, sheet$, text)

      case xml.ProcInstr(target, text) =>
           translateProcInstr(target, text)

      case xml.Comment(text) => Seq.empty

    }


  }

  def translatePCData(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] =
      translateText(paragraph, attributes, sheet, text)

  def translateProcInstr(target: String, text: String): Seq[Target] = Seq.empty
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

  def textStyleTranslation(textStyle: String): Translation = new Translation {
    override def translate(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, child: Seq[Node]): Seq[Target] =
      super.translate(paragraph, attributes.updated("textStyle", textStyle), sheet, child)
  }

  val translator: Translation = new Translation

  locally {
    translator("i")  = textStyleTranslation("Italic")
    translator("b")  = textStyleTranslation("Bold")
    translator("bi") = textStyleTranslation("BoldItalic")
    translator("n")  = textStyleTranslation("Normal")
  }


  def translate(source: Node): Unit = {
    import PrettyPrint.AnyPretty
    for { t <- (translator.translate(false, Map.empty, Sheet(), source)) } t.prettyPrint()
  }


  val h: Node  =
    <body xmlns:h="http://www.w3.org/TR/html4/" width="40em" face="Courier">
      the first body line
      <div class="level1" width="20em">
        <p class="level2"> this&today;is a very<i class="level3">interesting</i>'ed
            paragraph
            <bi>and this is bold italic text</bi><foo/></p>
        a <i>completely</i> level1 line
      </div>
      the last body lump&yesterday;what?
    </body>

  def main(args: Array[String]): Unit = {
    translate(h)
    println("-----")
  }

}

