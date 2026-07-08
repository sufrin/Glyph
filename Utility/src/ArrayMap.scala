package org.sufrin.utility


/**
 * View of an array as a mapping
 */
object ArrayMap {
   def apply[T](arr: Array[T]): collection.immutable.Map[Int, T] = new collection.immutable.AbstractMap[Int, T] {

      def get(key: Int): Option[T] = if (key<arr.length) Some(arr(key)) else None

      def iterator = arr.indices.iterator.map(i => i -> arr(i))

      @deprecated def removed(key: Int): Map[Int, T] = ???

      @deprecated def updated[V1 >: T](key: Int, value: V1): Map[Int, V1] = ???
   }
}
