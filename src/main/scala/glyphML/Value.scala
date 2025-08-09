package org.sufrin.glyph
package glyphML

import glyphML.Context.{AttributeMap, Env}

import org.sufrin.glyph.unstyled.static.INVISIBLE

import scala.util.matching.Regex

trait Value {
  val kind = this.getClass.getSimpleName
}

object Empty {
  val attributes = Attributes(Map.empty)
  val glyphValue = GlyphValue{ cxt => INVISIBLE() }
}

case class Attributes(attributes: AttributeMap) extends Value
case class GlyphValue(apply: Env => Glyph) extends Value
case class StringValue(string: String) extends Value

class ValueStore {
  private val store = collection.mutable.LinkedHashMap[(String, String), Value]()

  def update(name: String, value: Value): Unit = store((name, value.kind)) = value

  def apply(name: String, kind: String): Value = store((name, kind))

  def apply(name: Regex, kind: Regex): Iterator[((String, String), Value)] =
      store.iterator.filter{ case ((n,k),v) => name.matches(n) }.filter{ case ((n,k),v) => kind.matches(k) }

  def getLikeOrUpdate(value: Value)(name: String): Value = store.getOrElseUpdate((name, value.kind), value)

  def getLike(like: Value): String=>Option[Value] = {
    val kind = like.kind
    (name: String)=>store.get(name, kind)
  }

  def getLikeElse(value: Value): String=>Value =  {
    getLike(value) andThen {
      case None        => value
      case Some(value) => value
    }
  }
}