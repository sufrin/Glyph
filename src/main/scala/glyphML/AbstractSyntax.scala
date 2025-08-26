package org.sufrin.glyph
package glyphML


import org.sufrin.SourceLocation.{sourcePath, SourceLocation}

import scala.util.matching.Regex

/**
 * Heading towards a clearer method of translating our XML dialect to `Glyph`s.
 * Started, and good progress made (Aug '25 BS)
 *
 * The big idea is to do the semantic translation from
 * abstract syntax `Tree`s (as defined here) rather than
 * `scala.xml.Node`s.
 *
 * At present we recover the `Tree`s
 * from `scala.xml.Node`s, but we'd like to remove that
 * dependency because we don't really need all the
 * `scala.xml` paraphernalia which is oriented towards
 * a XML in general (but whose parser doesn't have
 * any way of tracking source locations).
 *
 * Texts are preprocessed into `Textual` trees by
 * splitting them into distinct chunks (of solid and whitespace characters), and
 * separating punctuation from "solid" chunks. The latter makes hyphenation
 * very straightforward
 *
 *
 */

object AbstractSyntax {

  import Context.AttributeMap

  case class Scope(tags: List[String]=Nil, definitionScope: Option[Scope]=None) {
    def nest(tag: String): Scope = Scope(tag::tags)
    override val toString: String = s"${tags.reverse.mkString("<")} ${if (definitionScope.isEmpty) "" else s"/ ${definitionScope.get}"}"
    def hasNested(tag: String): Boolean = tags.contains(tag)
    def definedIn(defn: Scope): Scope = Scope(tags, Some(defn))
  }

  trait Tree {
    val scope: Scope=Scope(Nil)
  }

  case class Element(override val scope: Scope, tag: String, attributes: AttributeMap, child: Seq[Tree]) extends Tree {
    import Context._
    override def toString:String = s"$scope<$tag [${attributes.mkString()}]>$child</>"
  }

  sealed trait Textual extends Tree
  case class Text(text: String)        extends Textual
  case class Para(texts: Seq[String])  extends Textual
  case class MacroParam(name: String)  extends Tree // we want to experiment with naming conventions
  case class Quoted(text: String)      extends Tree
  case class Entity(name: String)      extends Tree
  case class Comment(target: String, text: String) extends Tree

  private val chunk: Regex = """[^\n\s]+|[\n\s]+""".r
  /**
   * Slice a text into its chunks, unpacking punctuation adjacent to non-space chunks.
   *
   * TODO: make this more efficient.
   */
  def fromText(text: String): Textual = {
        val words: Seq[String] = chunk.findAllIn(text).toSeq
        if (words.length==1) Text(words.head) else Para(words)
  }

  def isEmptyText(tree: Tree): Boolean = tree match {
    case Text(text) => text.forall(_.isWhitespace)
    case Para(texts) => texts.forall(_.forall(_.isWhitespace))
    case Comment(_,_) => true
    case _ => false
  }

  /**
   * Maps a `scala.xml.Node` to a `Tree`, decorating each `Elem` with an appropriate
   * scope nest and attribute mapping, and normalizing attribute names to lowercase.
   */
  def fromXML(outerScope: Scope) (source: xml.Node): Tree = {
    source match {
      case xml.Elem(str, tag, attrs, binding, child@_*)   =>
           Element(outerScope, tag, attrs.asAttrMap.map { case (k, d) => (k.toLowerCase, d) }, child.map(fromXML(outerScope nest tag)))
      case xml.EntityRef(name)                            => Entity(name)
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Text(text)                                 => fromText(text)
      case xml.Comment(text: String)                      => Comment("", text)
      case xml.ProcInstr(target, "")                      => MacroParam(target)
      case xml.ProcInstr(target, text)                    => Comment(target, text)
      case atom: xml.Atom[Any]                            => fromText(atom.data.toString)
    }
  }

  def fromXML(source: xml.Node)(implicit location: SourceLocation = sourcePath): Tree = fromXML(Scope(List(location.toString)))(source)

  /*def main(args: Array[String]): Unit = {
      import org.sufrin.glyph.glyphXML.PrettyPrint._
      def t(source: xml.Node): Unit = fromXML(Scope())(source).prettyPrint()
      t(
        <outer ouTer="outer">
          The outer is outer
          <inner Inner="inner">outer and inner</inner>in the plain
          <inner Outer="inner" inner="outer">outer=inner inner=outer
            <furtherin further="further"/>
          </inner>
        </outer>
      )
      t(<div width="45em" align="justify" local="local">
          <p local="onlyP">
            This app solves the equation<i UPPER="foo">c = a + b</i>if at least &amp; two of <i>a, b, c</i>
            are well-formed numbers: possibly floating-point.
          </p>
          <![CDATA[equation <i UPPER="foo">c = a + b</i> if at]]>
          <p>
            Typing <tt>↩</tt> (<i>ie. the enter key</i>) in any of the text fields, causes the
            equation to be re-solved.
            <!-- A comment -->
          </p>
        </div>)
    }*/
}
