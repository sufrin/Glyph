package org.sufrin.glyph
package glyphML

import glyphML.AbstractSyntax.{Element, MacroParam, Scope, Tree}
import glyphML.Context._

import org.sufrin.logging.SourceDefault

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
 * <?body?>  => body
 * <?bodyN?> => the Nth non-blank child of body (N an integer)
 * }}}
 *
 * and each elements given `attrs` replaced by
 *
 * {{{
 *   attrs                supersede
 *   invocationAttributes supersede
 *   defaultAttributes
 * }}}
 *
 * @param scope the scope/location at which it was defined
 * @param tag the tag of elements that inoke the macro
 * @param attributes the attributes (default attributes) defined for the macro
 * @param macroBody the body of the macro
 */

case class Macro(val definitionScope: Scope, tag: String, val defaultAttributes: AttributeMap, context: Context, macroBody: Seq[Tree])  {
  import ExtendedAttributeMap._
  override def toString: String = s"Macro $tag $definitionScope ${defaultAttributes.mkString})"
  /**
   *  substitute invocation attributes and invocation body parts in appropriate places in the abstraction body
   *
   *  // TODO: get the scope right in macro expansions
   */
  def expansion(invocationAttributes: AttributeMap, invocationBody: Seq[Tree]): Seq[Tree] = {

    lazy val nonEmptyBody: Seq[Tree] = invocationBody.filterNot(AbstractSyntax.isEmptyText(_))

    def nonEmpty(i: Int): Seq[Tree] =
      if (i<nonEmptyBody.length)
        List(nonEmptyBody(i))
      else {
        SourceDefault.warn(s"No such nonempty element <?body$i?> expanding macro defined at $definitionScope<$tag")
        Nil
      }

    def substitute(tree: Tree): Seq[Tree] = tree match {
      case Element(scope, tag, attrs, body) =>
        List(Element(definitionScope.definedIn(scope), tag, attrs supersede invocationAttributes supersede defaultAttributes, body.flatMap(substitute)))
      case MacroParam("body") =>
            invocationBody
      case MacroParam(s"body$name") if name.matches("[0-9]+") =>
            nonEmpty(name.toInt)
      case MacroParam(name) =>
            List(tree)
      case other =>
            List(other)
    }


    macroBody.flatMap(substitute)
  }

  def expanded(translator: Translator, invocationAttributes: AttributeMap, invocationBody: Seq[Tree]): Seq[Glyph] = {
    val substituted = expansion(invocationAttributes, invocationBody)
    val derivedContext = context.updated(invocationAttributes supersede defaultAttributes)
    substituted.flatMap(translator.translate(derivedContext))
  }

}
