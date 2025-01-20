package org.sufrin.glyph
package gxml

import scala.xml.Node

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