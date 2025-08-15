package org.sufrin.glyph
package glyphML

import glyphML.AbstractSyntax.Scope
import glyphML.Context.AttributeMap
import unstyled.static.INVISIBLE

import org.sufrin.SourceLocation.{sourcePath, SourceLocation}

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
  val Macro           = StoredMacro(null)
}


case class StoredElement(element: AbstractSyntax.Element)   extends Value {
  override val toString: String = element.toString
}

case class StoredAttributeMap(attributes: AttributeMap)     extends Value {
  import Context._
  override val toString: String = s"[${attributes.mkString()}]"
}

case class StoredGlyphGenerator(apply: StyleSheet => Glyph) extends Value  {
  override val kind: String="Glyph"
  override val toString: String = "StoredGenerator"
}
case class StoredGlyphConstant(glyph: Glyph)                extends Value  {
  override val kind: String="Glyph"
  override val toString: String = "StoredConstant"
}

case class StoredString(string: String)                     extends Value
case class StoredMacro(theMacro: Macro)                     extends Value


class ValueStore { thisStore =>
  private val store = collection.mutable.LinkedHashMap[(String, String), Value]()

  def update(name: String, value: Value): Unit = store((name, value.kind)) = value

  def apply(name: String, kind: String): Value = store((name, kind))

  def apply(name: Regex, kind: Regex): Iterator[((String, String), Value)] =
      store.iterator.filter{ case ((n,k),v) => name.matches(n) }.filter{ case ((n,k),v) => kind.matches(k) }

  def show(name: Regex = ".*".r, kind: Regex=".*".r): Seq[String] =
      apply(name, kind).map{ case ((n,k),v) => s"$n: $k = $v" } . toSeq

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

  def update(name: String, thing: Macro): Unit             = thisStore(name) = StoredMacro(thing)
  def update(name: String, thing: AttributeMap): Unit      = thisStore(name) = StoredAttributeMap(thing)
  def update(name: String, thing: Glyph): Unit             = thisStore(name) = StoredGlyphConstant(thing)
  def update(name: String, thing: StyleSheet=>Glyph): Unit = thisStore(name) = StoredGlyphGenerator(thing)
  def update(name: String, thing: String): Unit            = thisStore(name) = StoredString(thing)


  override def toString: String = {
    store.toSeq.map{case ((k: String, t: String), d: Value)=>s"$k: $t -> $d"}.mkString(" ")

  }
}