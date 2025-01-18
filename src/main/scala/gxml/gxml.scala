package org.sufrin.glyph
package gxml


import org.sufrin.glyph.gxml.Visitor.AttributeMap

import scala.annotation.nowarn
import scala.collection.mutable
import scala.xml._



trait Visitor {
  import Visitor.AttributeMap
  def visitText(attributes: AttributeMap, text: String): Unit
  def visitElement(attributes: AttributeMap, tag: String, child: Seq[Node]): Unit = for {node <- child} visit(attributes, node)
  def visitEntity(attributes: AttributeMap, name: String): Unit
  def visitPCData(attributes: AttributeMap, text: String): Unit
  def visitProcInstr(attributes: AttributeMap, target: String, text: String): Unit = {}
  def visitComment(attributes: AttributeMap, text: String): Unit = {}

  def extendContextFor(attributes: AttributeMap, tag: String, attrMap: Map[String,String]): AttributeMap = attributes ++ attrMap

  def visit(attributes: AttributeMap, source: Node): Unit = {
    source match {
      case xml.EntityRef(name) => visitEntity(attributes, name)

      case xml.Elem(str, tag, attrs, binding, child@_*) =>
        val attributes$ : AttributeMap = extendContextFor(attributes, tag, attrs.asAttrMap)
        visitElement(attributes$, tag, child)

      case xml.Text(buffer) =>
        visitText(attributes, buffer.toString)

      case xml.Comment(text: String) =>
        visitComment(attributes, text)

      case xml.PCData(text: String) =>
        visitPCData(attributes, text)

      case xml.ProcInstr(target, text) =>
        visitProcInstr(attributes, target, text)

    }
  }
}

object Visitor {
  type AttributeMap = Map[String, String]
}

object PrettyPrint {

  implicit class XMLPretty (xobj: xml.Node) {
    def prettyPrint(): Unit = PrettyPrint.prettyPrint(XMLElem(xobj))
  }

  implicit class AnyPretty (obj: Any) {
    def prettyPrint(): Unit = PrettyPrint.prettyPrint(obj)
  }

  def XMLElem(xobj: xml.Node): PrettyPrintable =
    new PrettyPrintable {
      /** The name of the class (or object) */
      def prefix: String = xobj match {
        case xml.Elem(str, str1, data, binding, child @ _*) => s"<$str1$data>"
        case xml.Text(buffer) => s"\"${buffer.replaceAll("[\\n][ ]*", "⎆")}\""
        case xml.Comment(text) => s"<!--$text-->"
        case xml.EntityRef(text) => s"&$text;"
      }

      /** The number of fields/elements of the object */
      def arity: Int = xobj match {
        case xml.Elem(str, str1, data, binding, child @ _*) => child.length
        case _ => 0
      }

      override def field(i: Int): (String, Any) = xobj match {
        case Elem(str, str1, data, binding, child @ _*) => (i.toString, XMLElem(child(i)))
        case _ => ("", "")
      }
    }


  /**
   *  All objects of all structured classes can present a
   *  custom "face" to the pretty-printer.
   */
  trait PrettyPrintable {
    /** The name of the class (or object)  */
    def prefix:  String

    /** The number of fields/elements of the object */
    def arity:   Int

    /** The `i`'th field/element of the object as a name-value pair */
    def field(i: Int): (String, Any) = ("?", "?")

    def name(i: Int):  String = field(i)._1
    def value(i: Int): Any    = field(i)._2
  }

  @inline private def allPrim(p: Product): Boolean = p.productIterator.forall(isPrim)

  @inline private def isPrimTuple(p: Any): Boolean = p match {
    case obj : Tuple2[Any, Any]                     => allPrim(obj)
    case obj : Tuple3[Any, Any, Any]                => allPrim(obj)
    case obj : Tuple4[Any, Any, Any, Any]           => allPrim(obj)
    case obj : Tuple5[Any, Any, Any, Any, Any]      => allPrim(obj)
    case obj : Tuple6[Any, Any, Any, Any, Any, Any] => allPrim(obj)
    case _   => false
  }

  private def isPrim(obj: Any): Boolean =
    obj match {
      case _ : Int | _ : Long | _ : Char | _ : String | _ : Double | _ : Float => true
      case _ => false
    }

  private def isSingleton(obj: Any): Boolean =
    obj match {
      case prod : Product            => prod.productArity==1 && isPrim(prod.productElement(0))
      case _                         => false
    }

  /**
   *  Vertical bar -- indentation token for all
   *  but the last field/element of a product/sequence
   */
  val verticalBar = "\u2502 "
  /** Field indent -- always appears as the last indentation token on a line  */
  val fieldIndent  = "\u2514\u2500"

  /**
   * Pretty-prints a (possibly-structured) object
   *
   * 1. as itself if it is a (non-function) primitive
   *
   * 2. as the vertically-aligned fields of a case object if it is a product, unless
   *
   *  2.1 it is a product with a single primitive-valued field, in which case its `toString` is printed
   *
   *  2.2 or it is a tuple of primitive values, in which case its `toString` is printed
   *
   *
   * 3. as its vertically aligned elements, if it is an `Iterable`
   *
   *    3.1 prefixed by `[#${seq.length}]` if it is a sequence
   *
   *    3.2 prefixed by `...` if it is not a sequence
   *
   *
   * @param obj the object to be prettyprinted
   * @param lastInSeq is it being printed as the last element/field of a sequence or product
   * @param indentStack specification, in reverse order, of the indentation to be printed on each line
   * @param fieldName the field name (within a product) of the object if it is within a product, else `None`
   */
  def prettyPrint(obj: Any, lastInSeq: Boolean = true, indentStack: List[String] = List(), fieldName: Option[String] = None): Unit = {
    if (indentStack.length>15) return

    val indentToken = if (lastInSeq) "  " else verticalBar

    val prettyName  = fieldName.fold("")(x => s"$x: ") // name: or ""

    @nowarn("msg=non-variable") val prettyVal = obj match {
      case obj : PrettyPrintable              => obj.prefix
      case obj : Product if isPrimTuple(obj)  => obj.toString
      case obj : Seq[Any]                     => s"[#${obj.length}]"
      case _   : Iterable[Any]                => "..."

      case obj : Product => obj.productPrefix
      case _ : Function10[Any,Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
           |    _ : Function9[Any,Any, Any, Any, Any, Any, Any, Any, Any, Any]
           |    _ : Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any]
           |    _ : Function7[Any, Any, Any, Any, Any, Any, Any, Any]
           |    _ : Function6[Any, Any, Any, Any, Any, Any, Any]
           |    _ : Function5[Any, Any, Any, Any, Any, Any]
           |    _ : Function4[Any, Any, Any, Any, Any]
           |    _ : Function3[Any, Any, Any, Any]
           |    _ : Function2[Any, Any, Any]
           |    _ : Function1[Any, Any]       => "<fun>"
      case _                             => obj.toString
    }

    indentStack.foldRight(()){ case (l, _) => print(l) } // indent stack is in reverse
    print(s"$fieldIndent$prettyName$prettyVal")
    if (!isSingleton(obj)) println()

    obj match {
      case obj: PrettyPrintable =>
        val length = obj.arity
        for {i <- 0 until length - 1}
          prettyPrint(obj.value(i), false, indentToken :: indentStack, Some(obj.name(i)))
        if (length>0) prettyPrint(obj.value(length - 1), true, indentToken :: indentStack, Some(obj.name(length - 1)))

      case seq: Iterable[Any]   =>
      { val s = seq.toSeq
        for { i <-0 until s.length-1 }
          prettyPrint(s(i), false, "  " :: indentStack)
        prettyPrint(s(s.length-1), true, "  " :: indentStack)
      }

      case obj: Product if isPrimTuple(obj)  => // already printed

      case obj: Product  =>
        if (isSingleton(obj))
          println(s"(${obj.productElement(0)})")
        else
        { val length = obj.productArity
          for { i <-0 until length-1 }
            prettyPrint(obj.productElement(i), false, indentToken :: indentStack, Some(obj.productElementName(i)))
          prettyPrint(obj.productElement(length-1), true, indentToken :: indentStack, Some(obj.productElementName(length-1)))
        }

      case _ =>
    }
  }
}


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
  val generatorMap:     mutable.Map[String, Translation]    = mutable.LinkedHashMap[String, Translation]()
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
  def update(id: String, generator: Translation) = generatorMap(id) = generator
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


class Translation extends Primitives {
  import Visitor.AttributeMap

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

  trait Target
  case class Word(text: String) extends Target
  case object Space extends Target
  case class SimpleGlyph(glyph: Glyph) extends Target
  case class Para(chunks: Seq[Target]) extends Target


  def translateText(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, text: String): Seq[Target] = {
    def wordGlyph(text: String): Glyph = org.sufrin.glyph.Text(text, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush).asGlyph()
    def wordAtBaseline(text: String): Glyph = org.sufrin.glyph.Text(text, sheet.textFont, sheet.textForegroundBrush, sheet.textBackgroundBrush).atBaseline()

    if (paragraph) {
      val chunks = mutable.ArrayBuffer[Target]()
      def out(t: Target): Unit = chunks += t
      if (text.startsWith(" ") || text.startsWith("\n") || text.startsWith("\t")) out(Space)
      val it = text.split("[\t\n ]+").filterNot(_.isBlank).iterator // TODO: precompile the pattern
      while (it.hasNext) {
        out(SimpleGlyph(wordAtBaseline(it.next())))
        if (it.hasNext) out(Space)
      }
      if (text.endsWith(" ") || text.endsWith("\n") || text.endsWith("\t")) out(Space)

      chunks.toSeq
    } else {
      val lines = text.split("[\n]").toSeq.map(wordGlyph(_))
      List(SimpleGlyph(NaturalSize.Col(bg=sheet.textBackgroundBrush).atLeft$(lines)))
    }
  }


  def translate(paragraph: Boolean, attributes: AttributeMap, sheet: Sheet, source: Node): Seq[Target] = {
    source match {
      case xml.EntityRef(name) => translateText(paragraph, attributes, sheet, entityMap.getOrElse(name, s"&name;"))

      case xml.Elem(str, tag, localAttrs, binding, child@_*) =>
        val attributes$ : AttributeMap = extendContextFor(tag)(attributes, localAttrs.asAttrMap)
        tag match {
          case "p" =>
            val chunks = child.flatMap { source => translate(true, attributes$, sheet, source) }
            // MAKE CHUNKS A PARAGRAPH GLYPH
            List(Para(chunks))

          case other =>
            child.flatMap { source => translate(false, attributes$, sheet, source) }
        }

      case xml.Text(text) =>
        translateText(paragraph, attributes, sheet, text)

      case xml.PCData(text: String) =>
        translateText(paragraph, attributes, sheet, text)

      case xml.ProcInstr(target, text) => Seq.empty

      case xml.Comment(text) => Seq.empty

    }
}


}


object gxml {
  import xml._

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

  val translator: Translation = new Translation

  def translate(source: Node): Unit = {
    import PrettyPrint.AnyPretty
    for { t <- (translator.translate(false, Map.empty, Sheet(), source)) } println(t)
  }


  val h: Node  =
    <body xmlns:h="http://www.w3.org/TR/html4/" width="40em" face="Courier">
      the first body line
      <div class="level1" width="20em">
        <p class="level2"> this&today;is a very<i class="level3">interesing</i>'ed
            paragraph
          <foo/>
        </p>
        a level1 line
      </div>
      the last body lump&yesterday;what?
    </body>

  def main(args: Array[String]): Unit = {
    //flatten(h)
    //println("-----")
    translate(h)
    println("-----")
    //println(h)
    //println("-----")
    //import PrettyPrint.XMLPretty
    //h.prettyPrint()
  }

}

