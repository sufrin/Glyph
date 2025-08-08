package org.sufrin.glyph
package glyphML


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

  trait Tree {
    
  }

  case class Element(tag: String, attributes: AttributeMap, child: Seq[Tree]) extends Tree

  sealed trait Textual extends Tree
  case class Text(text: String)        extends Textual
  case class Para(texts: Seq[String])  extends Textual

  case class Quoted(text: String) extends Tree

  case class Entity(name: String) extends Tree

  case class Comment(target: String, text: String) extends Tree

  private val chunk: Regex = """[^\n\s]+|[\n\s]+""".r
  private val punct: Regex = """[^\.\";:',!?]+|[\.\";:',!?]+""".r
  private val punctuation  = ".\";:',!?".toList.map(_.toString)
  private def isPunctuated(word: String): Boolean = { // pre: word.nonEmpty
    !word.head.isWhitespace && punctuation.exists{p => word.startsWith(p) ||  word.endsWith(p) }
  }

  /**
   * Slice a text into its chunks, unpacking punctuation adjacent to non-space chunks.
   *
   * TODO: make this more efficient.
   */
  def fromText(text: String): Textual = {
        val chunks: Seq[String] = chunk.findAllIn(text).toSeq
        val words =
          chunks.flatMap {
            chunk => if (isPunctuated(chunk)) punct.findAllIn(chunk).toSeq else List(chunk)
        }
        if (words.length==1) Text(words.head) else Para(words)
      }



  /**
   * Maps a `scala.xml.Node` to a `Tree`, decorating each `Elem` with an appropriate
   * attribute mapping, and normalizing attribute names to lowercase.
   */
  def fromXML(source: xml.Node): Tree = {
    import Context.ExtendedAttributeMap
    source match {
      case xml.EntityRef(name)                            => Entity(name)
      case xml.Elem(str, tag, attrs, binding, child@_*)   => Element(tag, attrs.asAttrMap.map { case (k, d) => (k.toLowerCase, d) }, child.map(fromXML))
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Text(text)                                 => fromText(text)
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Comment(text: String)                      => Comment("", text)
      case xml.ProcInstr(target, text)                    => Comment(target, text)
    }
  }

  def main(args: Array[String]): Unit = {
    import org.sufrin.glyph.glyphXML.PrettyPrint._
    def t(source: xml.Node): Unit = fromXML(source).prettyPrint()
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
  }

}
