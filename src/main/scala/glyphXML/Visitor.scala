package org.sufrin.glyph
package glyphXML

import scala.xml.Node

trait Visitor {
  import Visitor.AttributeMap
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
  type AttributeMap = Map[String, String]
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