package org.sufrin.glyph
package glyphML

import glyphML.AbstractSyntax._
import glyphML.Context._

import org.sufrin.logging.SourceDefault

import scala.collection.immutable.ListMap

/**
 * Representation of the body of a macro defined by
 *
 * &lt;macro tag=macrotag defaultattributes>
 *   macroBody
 * &lt;/macro>
 *
 * In general, in the scope of such a definition, an invocation of the form
 *
 *  &lt;macrotag invocationAttributes>
 *   body
 *  &lt;/macrotag>
 *
 * has the same effect (and result) as if the `macroBody` had
 * parameter substitutions done in it as follows:
 *
 * {{{
 * <?body?>   => body
 * <?body.N?> => the Nth non-blank child of body (N an integer)
 * }}}
 *
 * and each element's given `attrs` replaced by
 *
 * {{{
 *   expandedattrs        supersede
 *   invocationAttributes supersede
 *   defaultAttributes
 * }}}
 *
 * where `expandedattrs` maps each `(k,v)` of attrs to itself, unless `v` is `?key'` (ie starts with a
 * `?`; in which case it is replaced by the result of looking up `key'` in
 * {{{
 *     invocationAttributes supersede
 *     defaultAttributes
 * }}}
 *
 * If the lookup fails (ie there is no `key'` in the invocation or the default attributes, then
 * the original `(k,v)` does not appear in `expandedattrs`.
 *
 * @param scope the scope/location at which it was defined
 * @param tag the tag of elements that inoke the macro
 * @param attributes the default attributes defined for invocations of the macro
 * @param macroBody the body of the macro
 */

case class Macro(val definitionScope: Scope, tag: String, val defaultAttributes: AttributeMap, context: Context, macroBody: Seq[Tree])  {
  import ExtendedAttributeMap._
  override def toString: String = s"Macro $tag $definitionScope ${defaultAttributes.mkString})"
  /**
   *  substitute invocation and default attributes and invocation body parts in appropriate places in the abstraction body
   */
  def expansion(invocationAttributes: AttributeMap, invocationBody: Seq[Tree]): Seq[Tree] = {

    lazy val nonEmptyBody: Seq[Tree] = invocationBody.flatMap(topmostNonempty)

    def nonEmpty(i: Int): Seq[Tree] =
      if (i<nonEmptyBody.length)
        List(nonEmptyBody(i))
      else {
        SourceDefault.warn(s"No such nonempty element <?body$i?> expanding macro defined at $definitionScope<$tag")
        Nil
      }

    def substitute(tree: Tree): Seq[Tree] = tree match {
      case Element(scope, tag, attrs, body) =>
        val contextAttributes = invocationAttributes supersede defaultAttributes
        val expandedAttrs = attrs.flatMap {
          case (k, s"?$v") =>
            contextAttributes.get(v) match {
              case None             => Nil
              case Some(substitute) => List((k, substitute))
            }
          case other => List(other)
        }
        List(Element(definitionScope.definedIn(scope), tag, ListMap.from(expandedAttrs) supersede contextAttributes, body.flatMap(substitute)))
      case MacroParam("body") =>
            invocationBody
      case MacroParam(s"body.$name") if name.matches("[0-9]+") =>
           nonEmpty(name.toInt)
      case MacroParam(s"body($name)") if name.matches("[0-9]+") =>
           nonEmpty(name.toInt)
      case MacroParam(s"body$name") if name.matches("[0-9]+") =>
           nonEmpty(name.toInt)
      case MacroParam(name) =>
            val s = (invocationAttributes supersede defaultAttributes).String(name, "")
            if (s.isEmpty) List(tree) else List(AbstractSyntax.Text(s))
      case other =>
            List(other)
    }

    macroBody.flatMap(substitute)
  }

  def expanded(translator: Translator, invocationContext: Context, invocationBody: Seq[Tree]): Seq[Glyph] = {
    import glyphML.Context.ExtendedAttributeMap
    val invocationAttributes = invocationContext.attributes
    val trace = invocationAttributes.Bool("trace", false)
    val substituted = expansion(invocationAttributes, invocationBody)
    if (trace) {
      println(s"EXPANDED  <$tag ${invocationAttributes.mkString()} >")
      //println(PrettyPrint.prettyPrint(invocationBody))
      println(PrettyPrint.prettyPrint(substituted))
    }
    val derivedContext = context.updated(invocationAttributes.without("trace") supersede defaultAttributes, tag)
    substituted.flatMap(translator.translate(derivedContext))
  }

}
