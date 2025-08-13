package org.sufrin.glyph
package glyphML

import glyphML.AbstractSyntax.{Scope, Tree, Element}
import glyphML.Context._

case class Macro(val scope: Scope, tag: String, attributes: AttributeMap, body: Seq[Tree])  {
  /**
   *  substitute attributes in the body of the macro, and name "pieces" of
   *  the invocation.
   */
  def expansion(invocationAttributes: AttributeMap, invocation: Seq[Tree]): Seq[Tree] = {
    def substitute(tree: Tree): Tree = tree match {
      case Element(scope, tag, attrs, body) =>
        val newAttrs = invocationAttributes supersede attributes
        Element(scope, tag, newAttrs, body.map(substitute))
      case other => other
    }
    body.map(substitute)
  }
}
