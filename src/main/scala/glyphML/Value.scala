package org.sufrin.glyph
package glyphML

import glyphML.Context.{AttributeMap, Env}

import org.sufrin.glyph.unstyled.static.INVISIBLE
import org.sufrin.SourceLocation.{sourcePath, SourceLocation}
import org.sufrin.glyph.glyphML.AbstractSyntax.Scope

import scala.util.matching.Regex

trait Value {
  val kind = this.getClass.getSimpleName
}

/**
 * Prototypes/default values
 */

object StoreType {
  val AttributeMap    = StoredAttributeMap(Map.empty)
  val GlyphGenerator  = StoredGlyphGenerator{ cxt => INVISIBLE() }
  val GlyphConstant   = StoredGlyphConstant{ INVISIBLE() }
  val String          = StoredString("")
  val Element         = StoredElement(AbstractSyntax.Element(scope=Scope(List(sourcePath.toString)), tag="", attributes=Map.empty, child=Nil))
}


case class StoredElement(element: AbstractSyntax.Element) extends Value
case class StoredAttributeMap(attributes: AttributeMap) extends Value
case class StoredGlyphGenerator(apply: StyleSheet => Glyph) extends Value  { override val kind: String="Glyph"}
case class StoredGlyphConstant(glyph: Glyph)                extends Value  { override val kind: String="Glyph"}
case class StoredString(string: String) extends Value


class ValueStore { thisStore =>
  private val store = collection.mutable.LinkedHashMap[(String, String), Value]()

  def update(name: String, value: Value): Unit = store((name, value.kind)) = value

  def apply(name: String, kind: String): Value = store((name, kind))

  def apply(name: Regex, kind: Regex): Iterator[((String, String), Value)] =
      store.iterator.filter{ case ((n,k),v) => name.matches(n) }.filter{ case ((n,k),v) => kind.matches(k) }

  def getKindElseUpdate(value: Value)(name: String): Value = store.getOrElseUpdate((name, value.kind), value)

  def getKind(like: Value): String=>Option[Value] = {
    val kind = like.kind
    (name: String)=>store.get(name, kind)
  }

  def getKindElse(value: Value): String=>Value =  {
    getKind(value) andThen {
      case None        => value
      case Some(value) => value
    }
  }

  def remove(like: Value)(name: String): Unit = {
    store.remove((like.kind, name))
  }

  /** Storing an element or some attributes */
  def update(name: String, scalaelem: scala.xml.Elem)(implicit location: SourceLocation = sourcePath): Unit = {
    scalaelem.label match {
      case "attributes" =>
        thisStore (name) = StoredAttributeMap ( scalaelem.attributes.asAttrMap )
      case _  =>
        AbstractSyntax.fromXML(scalaelem)((location))  match {
          case elem: AbstractSyntax.Element => thisStore (name) = StoredElement (elem)
        }
    }
  }
  def update(name: String, thing: AttributeMap): Unit      = thisStore(name) = StoredAttributeMap(thing)
  def update(name: String, thing: Glyph): Unit             = thisStore(name) = StoredGlyphConstant(thing)
  def update(name: String, thing: StyleSheet=>Glyph): Unit = thisStore(name) = StoredGlyphGenerator(thing)
  def update(name: String, thing: String): Unit            = thisStore(name) = StoredString(thing)

}