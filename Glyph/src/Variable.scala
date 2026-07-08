package org.sufrin.glyph

/**
 *  An "active" variable that triggers a method
 *  when its `value=` method is invoked .
 */
class Variable[T](
    initially: T,
    onChange: (T, T) => Unit = { (oldv: T, newv: T) => {} }
) extends Settable [T] {
  private var _theValue: T = initially

  override def toString: String = s"${_theValue}"

  /** Set the `value` of the variable
   *  @see value_=
   */
  def set(value: T): Unit = this.value = value

  /** Yield the value of the variable */
  def get: T = _theValue

  /** Yield the value of the variable */
  def value: T = _theValue

  /**
   * Set the value of the variable, and
   * trigger `onChange(oldValue, value)`.
   */
  def value_=(value: T): Unit = {
    val oldValue = _theValue
    _theValue = value
    if ((_theValue != null) /*&& (_theValue != value)*/) onChange(oldValue, value)
  }

  def apply(changeTo: T => Unit): Variable[T] = new Variable(initially, onChange = { case (from, to) => changeTo(to) })
}

/**
 * An active `Variable[Boolean]` that triggers a reaction when its
 * value changes. As a convenience it enables a list of things to
 * be registered with it.
 */
class BooleanVariable[T](initially: Boolean, reaction: Boolean => Unit) extends
      Variable[Boolean](initially, { (_, state) => reaction(state) }) {
  val registered = new collection.mutable.ListBuffer[T]()
  def register(thing: T): Unit = registered.addOne(thing)
  def remove(thing: T): Unit = registered.filter{elt: T => elt != thing}
}

object Variable {
  def apply[T](initially: T): Variable[T] = new Variable[T](initially)
}

object BooleanVariable {
  def apply[T](initially: Boolean)(reaction: Boolean =>Unit ): BooleanVariable[T] =
    new BooleanVariable[T](initially, reaction)
}


trait Settable[T] {
  def set(state: T): Unit
  def get: T
}
