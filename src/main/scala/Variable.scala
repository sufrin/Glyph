package org.sufrin.glyph

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

object Variable {
  def apply[T](initially: T): Variable[T] = new Variable[T](initially)
}

trait Settable[T] {
  def set(state: T): Unit
  def get: T
}
