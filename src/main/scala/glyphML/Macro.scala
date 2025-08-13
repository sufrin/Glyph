package org.sufrin.glyph
package glyphML

import glyphML.AbstractSyntax.{Scope, Tree}
import glyphML.Context.AttributeMap

case class Macro(val scope: Scope, tag: String, attributes: AttributeMap, child: Seq[Tree])  {
}
