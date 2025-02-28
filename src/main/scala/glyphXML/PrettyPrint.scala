package org.sufrin.glyph
package glyphXML

import scala.annotation.nowarn
import scala.xml.Elem

object PrettyPrint {

  implicit class XMLPretty (xobj: xml.Node) {
    def prettyPrint(): Unit = PrettyPrint.prettyPrint(XMLElem(xobj))
  }

  implicit class AnyPretty (obj: Any) {
    def prettyPrint(): Unit = PrettyPrint.prettyPrint(obj)
  }

  def XMLElem(xobj: xml.Node): PrettyPrintable =
    new PrettyPrintable {
      /** The name ofPaint the class (or object) */
      def prefix: String = xobj match {
        case xml.Elem(str, str1, data, binding, child @ _*) => s"<$str1$data>"
        case xml.Text(buffer) => s"\"${buffer.replaceAll("[\\n][ ]*", "⎆")}\""
        case xml.Comment(text) => s"<!--$text-->"
        case xml.EntityRef(text) => s"&$text;"
      }

      /** The number ofPaint fields/elements ofPaint the object */
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
   *  All objects ofPaint all structured classes can present a
   *  custom "face" to the pretty-printer.
   */
  trait PrettyPrintable {
    /** The name ofPaint the class (or object)  */
    def prefix:  String

    /** The number ofPaint fields/elements ofPaint the object */
    def arity:   Int

    /** The `i`'th field/element ofPaint the object as a name-value pair */
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
   *  but the last field/element ofPaint a product/sequence
   */
  val verticalBar = "\u2502 "
  /** Field indent -- always appears as the last indentation token on a line  */
  val fieldIndent  = "\u2514\u2500"

  /**
   * Pretty-prints a (possibly-structured) object
   *
   * 1. as itself if it is a (non-function) primitive
   *
   * 2. as the vertically-aligned fields ofPaint a case object if it is a product, unless
   *
   *  2.1 it is a product with a single primitive-valued field, in which case its `toString` is printed
   *
   *  2.2 or it is a tuple ofPaint primitive values, in which case its `toString` is printed
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
   * @param lastInSeq is it being printed as the last element/field ofPaint a sequence or product
   * @param indentStack specification, in reverse order, ofPaint the indentation to be printed on each line
   * @param fieldName the field name (within a product) ofPaint the object if it is within a product, else `None`
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
        if (s.nonEmpty) {
          for {i <- 0 until s.length - 1}
            prettyPrint(s(i), false, "  " :: indentStack)
          prettyPrint(s(s.length - 1), true, "  " :: indentStack)
        } else {

        }
      }

      case obj: Product if isPrimTuple(obj)  => // already printed

      case obj: Product  =>
        if (isSingleton(obj))
          println(s"(${obj.productElement(0)})")
        else
        { val length = obj.productArity
          for { i <-0 until length-1 }
            prettyPrint(obj.productElement(i), false, indentToken :: indentStack, Some(obj.productElementName(i)))
          if (length>0) prettyPrint(obj.productElement(length-1), true, indentToken :: indentStack, Some(obj.productElementName(length-1)))
        }

      case _ =>
    }
  }
}
