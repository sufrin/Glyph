package org.sufrin.glyph
package glyphXML


import scala.util.matching.Regex

/**
 * Heading towards a clearer method of translating `glyphXML`.
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
 * Unfinished (Aug '25 BS)
 */

object AbstractSyntax {

  import Context.AttributeMap

  trait Tree {
  }

  case class Element(tag: String, attributes: AttributeMap, child: Seq[Tree]) extends Tree

  sealed trait Textual extends Tree
  case class Text(text: String)   extends Textual
  case class Para(texts: Seq[String])  extends Textual

  case class Quoted(text: String) extends Tree

  case class Entity(name: String) extends Tree

  case class Comment(target: String, text: String) extends Tree

  private val atSpace: Regex = """[^\n\s]+""".r

  def sliceText(text: String): Textual = {
      if (text.exists(_.isSpaceChar)) {
        val first = text.head.isSpaceChar
        val last  = text.last.isSpaceChar
        val words: Seq[String] = atSpace.findAllIn(text).toSeq
        val w1: Seq[String] = if (first) f"${text.head}%c" +: words else words
        val lastString: String = f"${text.last}%c"
        val w2: Seq[String] = if (last) w1 :+ lastString else w1
        if (w2.length==1) Text(w2.head) else Para(w2)
      }
      else
        Text(text)
  }


  /**
   * Maps a `scala.xml.Node` to a `Tree`, decorating each `Elem` with an appropriate
   * attribute mapping, and normalizing attribute names to lowercase.
   *
   * Each `Elem` is decorated with its own attributes over the inherited attributes its tag
   * does not require it to refuse.
   *
   * Its legacy to its descendants is its decoration, excluding any attributes its tag requires it to
   * contain.
   */
  def fromXML(refuse: Map[String, Seq[String]], contain: Map[String, Seq[String]], inherited: AttributeMap)(source: xml.Node): Tree = {
    import Context.ExtendedAttributeMap
    source match {
      case xml.EntityRef(name)                            => Entity(name)
      case xml.Elem(str, tag, attrs, binding, child@_*)   =>
        val localAttrs = refuse.get(tag) match {
          case None           => attrs.asAttrMap.map{ case (k, d) => (k.toLowerCase, d) }
          case Some(refused)  =>
            attrs.asAttrMap.map{ case (k, d) => (k.toLowerCase, d) }.removedAll(refused)
        }
        val legacy = contain.get(tag) match {
          case None            => localAttrs.supersede(inherited)
          case Some(keepLocal) => localAttrs.supersede(inherited).removedAll(keepLocal)
        }
        Element(tag, localAttrs, child.map(fromXML(refuse, contain, legacy)))
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Text(text)                                 => sliceText(text)//Text(text)
      case xml.PCData(text: String)                       => Quoted(text)
      case xml.Comment(text: String)                      => Comment("", text)
      case xml.ProcInstr(target, text)                    => Comment(target, text)
    }
  }

  def fromXML(source: xml.Node): Tree = fromXML(Map.empty, Map.empty, Map.empty)(source)

  def main(args: Array[String]): Unit = {
    val refuse: Map[String, Seq[String]] =  collection.immutable.Map(
      "p" -> List("local"),
      "i" -> List("width", "align")
      )
    val contain: Map[String, Seq[String]] =  collection.immutable.Map(
      "p" -> List("width", "height")
      )
    import PrettyPrint.AnyPretty
    def t(source: xml.Node): Unit = fromXML(refuse, contain, Map.empty)(source).prettyPrint()
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
