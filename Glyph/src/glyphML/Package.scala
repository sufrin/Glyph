package org.sufrin.glyph
package glyphML

/**
 * Extensions are added to the definitions in a glyphML  `Translator`
 * @see Translator.withExtensions
 */
trait Package {
  def define(primitives: Definitions): Unit
}