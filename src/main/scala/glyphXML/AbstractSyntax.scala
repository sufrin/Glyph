package org.sufrin.glyph
package glyphXML

object AbstractSyntax {

  case class Context(attributes: Visitor.AttributeMap, sheet: StyleSheet, text: Boolean)

  trait Tree {
  }

  case class Element(tag: String, attributes: Visitor.AttributeMap, child: Seq[Tree]) extends Tree

  case class Text(text: String)   extends Tree

  case class Quoted(text: String) extends Tree

  case class Entity(name: String) extends Tree

  case class Comment(target: String, text: String) extends Tree
  def fromXML(source: xml.Node): Tree = {
    source match {
      case xml.EntityRef(name)                            => Entity(name)
      case xml.Elem(str, tag, attrs, binding, child@_*)   => Element(tag, attrs.asAttrMap, child.map(fromXML))
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Text(text)                                 => Text(text)
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Comment(text: String)                      => Comment("", text)
      case xml.ProcInstr(target, text)                    => Comment(target, text)
    }
  }

  def main(args: Array[String]): Unit = {
    import PrettyPrint.AnyPretty
    def t(source: xml.Node): Unit = fromXML(source).prettyPrint()
    t(
      <outer bar="x">
        The rain in spain
        <inner>falls mainly</inner>
        in the plain
      </outer>
    )
    t(<div width="45em" align="justify">
        <p>
          This app solves the equation <i UPPER="foo">c = a + b</i> if at least &amp; two of <i>a, b, c</i>
          are well-formed numbers: possibly floating-point.
        </p>
        <p>
          Typing <tt>↩</tt> (<i>ie. the enter key</i>) in any of the text fields, causes the
          equation to be re-solved.
          <!-- A comment -->
        </p>
      </div>)
  }

}
