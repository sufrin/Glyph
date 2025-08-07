package org.sufrin.glyph
package glyphXML

import org.sufrin.glyph.glyphXML.Translation.AttributeMap

import scala.xml.Node

trait Visitor {
  import Context.AttributeMap

  def visitText(attributes: AttributeMap, text: String): Unit
  def visitElement(attributes: AttributeMap, tag: String, child: Seq[Node]): Unit = for {node <- child} visit(attributes, node)
  def visitEntity(attributes: AttributeMap, name: String): Unit
  def visitPCData(attributes: AttributeMap, text: String): Unit
  def visitProcInstr(attributes: AttributeMap, target: String, text: String): Unit = {}
  def visitComment(attributes: AttributeMap, text: String): Unit = {}

  def extendFor(tag: String)(attributes: AttributeMap, attrMap: Map[String,String]): AttributeMap = attributes ++ attrMap

  def visit(attributes: AttributeMap, source: Node): Unit = {
    source match {
      case xml.EntityRef(name) => visitEntity(attributes, name)

      case xml.Elem(str, tag, attrs, binding, child@_*) =>
        val attributes$ : AttributeMap = extendFor(tag)(attributes, attrs.asAttrMap)
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
  import Context.AttributeMap


  def over(l: AttributeMap, r: AttributeMap): AttributeMap = new AttributeMap {

    def removed(key: String): Map[String, String] = over(l.removed(key), r.removed(key))

    def removedAll(keys: String*): Map[String, String] = over(l.removedAll(keys), r.removedAll(keys))

    def get(key: String): Option[String] = l.get(key) orElse(r.get(key))

    override def contains(d: String): Boolean = l.contains(d) || r.contains(d)

    override def keysIterator: Iterator[String] =
      l.keysIterator.concat(r.keysIterator.filterNot(l.contains(_)))

    def iterator: Iterator[(String, String)] =
      l.iterator.concat(r.filterNot{ case (d, r) => l.contains(d) })

    def updated[V1 >: String](key: String, value: V1): Map[String, V1] = ???
  }

  def toString(attributes: AttributeMap): String = attributes.map { case (k,d) => s"$k->$d"}.mkString(", ")

  def showNode(node: Node): Unit = {
    val v = new Visitor {

      def visitText(attributes: AttributeMap, text: String): Unit = println(s"${Visitor.toString(attributes)}: \"${text.replaceAll("[\\n][ ]*", "⎆")}\"")

      def visitEntity(attributes: AttributeMap, name: String): Unit = println(s"${Visitor.toString(attributes)}: &$name;")

      override def visitElement(attributes: AttributeMap, tag: String, child: Seq[Node]): Unit = {
        println(s"<$tag")
        super.visitElement(attributes, tag, child)
      }

      override def extendFor(tag: String)(attributes: AttributeMap, attrMap: Map[String,String]): AttributeMap = {
        val map$ : Map[String,String] = attrMap.map{ case (k, d) => (k, s"$tag:$d")}
        attributes ++ map$
      }

      override def visitPCData(attributes: AttributeMap, text: String): Unit = visitText(attributes, s"<![PCDATA[$text]]")

      override def visitProcInstr(attributes: AttributeMap, target: String, text: String): Unit = {}
    }
    v.visit(Map.empty, node)
  }

}