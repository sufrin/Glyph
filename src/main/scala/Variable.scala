package org.sufrin.glyph

import BooleanGlyphs.OnOffButton

/** Reference to a variable */
class Variable[T](
    initially: T,
    onChange: (T, T) => Unit = { (oldv: T, newv: T) => {} }
) {
  private var _theValue: T = initially

  override def toString: String = s"${_theValue}"

  def set(value: T): Unit = this.value = value

  def value: T = _theValue

  def value_=(value: T): Unit = {
    if ((_theValue != null) && (_theValue != value)) onChange(_theValue, value)
    _theValue = value
  }

  def get: T = _theValue

}

class ToggleVariable(initially: Boolean, reaction: Boolean => Unit) extends Variable[Boolean](initially, { (_, state) => reaction(state) }) {
  val buttons = new collection.mutable.ListBuffer[OnOffButton]()
  def addButton(button: OnOffButton): Unit = buttons.addOne(button)
}

object Variable {
  def apply[T](initially: T): Variable[T] = new Variable[T](initially)
  def reactive[T](initially: T)(reaction: (T,T)=>Unit ): Variable[T] = new Variable(initially, reaction)
}

object ToggleVariable {
  def apply[T](initially: Boolean)(reaction: Boolean =>Unit ): ToggleVariable = new ToggleVariable(initially, reaction)
}


trait Settable[T] {
  def set(state: T): Unit
  def get: T
}
