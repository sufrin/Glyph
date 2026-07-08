package org.sufrin.utility
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MultiMap[K, V](initially: (K,V)*){
  private val m: mutable.Map[K, ListBuffer[V]] = new mutable.LinkedHashMap[K,ListBuffer[V]]()

  locally {
    for { (k,v) <- initially } update(k, v)
  }

  def update(k: K, v: V): Unit = {
    val b = m.getOrElseUpdate(k, new ListBuffer())
    b.append(v)
  }

  def apply(k: K): Seq[V] =
    m.get(k) match {
      case None    => Nil
      case Some(b) => b.toSeq
    }

  def keys: Seq[K] = m.keys.toSeq

  override def toString: String = {
    val s = new StringBuilder()
    for { key <- m.keys } {
      s append key.toString
      s append "="
      s append apply(key).map(_.toString).mkString("[", "; ", "]")
    }
    s.toString
  }
}

object MultiMap {
  def apply[K, V](): MultiMap[K, V] = new MultiMap[K, V]()
}
