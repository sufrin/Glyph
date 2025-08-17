package org.sufrin.glyph
package glyphML

import glyphML.Context.Context
import GlyphTypes.Scalar

object DecorativeExtensions {
  def apply(implicit primitives: Definitions): DecorativeExtensions = new DecorativeExtensions(primitives)
}

class DecorativeExtensions(primitives: Definitions) {

  def turn(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
    import resolved._
    val degrees: Int = inheritedAttributes.Int("degrees", inheritedAttributes.Int("deg", 90*inheritedAttributes.Int("quads", 0)))
    def turn(glyph: Glyph): Glyph =
      degrees match {
        case 0 => glyph
        case d =>
          val glyph$ = glyph.turned(d.toFloat, false)
          glyph$
      }
    val derivedContext: Context = context.updated(inheritedAttributes.without("deg", "degrees", "quads"))
    val glyph = turn(NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext))))
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def scale(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
    import resolved._
    val proportion = inheritedAttributes.Float("scale", 0)
    val derivedContext: Context = context.updated(inheritedAttributes.without("proportion"))
    val glyph = NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext))).scaled(proportion)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

  def frame(translator: Translator)(context: Context)(element: AbstractSyntax.Element): Seq[Glyph] = {
    val resolved = new ResolveScopedAttributes(primitives, element.scope, element.tag, element.attributes, element.child)
    import glyphML.Context.{TypedAttributeMap,ExtendedAttributeMap}
    import resolved._
    val radius: Scalar = inheritedAttributes.Float("radius", 0)
    val fg  = inheritedAttributes.Brush("fg", inheritedAttributes.Brush("frameforeground", Brushes.black))
    val bg  = inheritedAttributes.Brush("bg", inheritedAttributes.Brush("framebackground", Brushes.transparent))
    val derivedContext: Context = context.updated(inheritedAttributes.without("fg", "bg"))
    val unframed = NaturalSize.Row(align=Mid)(children.flatMap(translator.translate(derivedContext)))
    val glyph = if (radius==0f) unframed.framed(fg, bg, radius = radius) else unframed.roundFramed(fg, bg, radius = radius)
    List(glyph.withBaseline(0.5f *(glyph.h + context.sheet.exHeight)))
  }

}
