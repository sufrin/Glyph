package org.sufrin.utility

import scala.collection.{immutable, mutable, Seq}

/** Generalization of `CharSequenceMap` to support `CodePointSeq` rather than just `String` */
class CodePointSeqMap[T]  extends mutable.Map[CodePointSeqMap.CodePointSeq, T] {
  import CodePointSeqMap._
  private var suffixes: immutable.Map[CodePoint, CodePointSeqMap[T]] = immutable.Map.empty
  private var value: Option[T] = None

  def get(s: CodePointSeq): Option[T] = get(s, 0)

  /** Pre:    0<=from<=s.length
   * Return: if (suffix in dom map) then Some(map(suffix)) else None
   * where suffix = s drop from
   */
  private def get(s: CodePointSeq, from: Int): Option[T] =
    if (from == s.length)
      value
    else
      suffixes get (s(from)) flatMap (_.get(s, from + 1))

  /**
   * Yield `Some(t, i)` where `t` is the value of the mapping at longest prefix of `s` that
   * matches one of the mapping's domain elements, and `i` is the length
   * of that prefix. If there is no such prefix, yield `None`.
   */
  def longestPrefixMatch(s: CodePointSeq): Option[(T, Int)] = longestPrefixMatch(forwardIterator(s))

  /**
   * Yield `Some(t, i)` where `t` is the value of the mapping at longest suffix of {{{s take upTo}}} that
   * matches one of the mapping's domain elements, and `i` is the length
   * of that suffix.  If there is no such prefix, yield `None`.
   */
  def longestSuffixMatch(s: CodePointSeq, upTo: Int): Option[(T, Int)] = longestPrefixMatch(reversedIterator(s, upTo))

  /**
   * Yield `Some(t, i)` where `t` is the value of the mapping at longest prefix of `it` that
   * matches one of the mapping's domain elements, and `i` is the length
   * of that prefix. If there is no such prefix, yield `None`.
   *
   */
  def longestPrefixMatch(it: Iterator[CodePoint]): Option[(T, Int)] = { // Inv:
    //   result==Some(t, n) =>
    //     n <= edges      &&
    //     map(it take n toString)==t &&
    //     there is no i st. n<i<edges && map(it take i toString)==t
    //
    var result: Option[(T, Int)] = None

    // if non-null, the node reached from the root by the path so far traversed
    // namely `it take edges`.
    var node: CodePointSeqMap[T] = this

    // the number of edges so far traversed
    var edges = 0
    while (it.hasNext && (node ne null)) {
      if (node.value.nonEmpty) result = Some(node.value.get, edges)
      node = node.suffixes.getOrElse(it.next(), null)
      edges += 1
    }
    if ((node ne null) && node.value.nonEmpty) result = Some(node.value.get, edges)
    result
  }


  /**
   * Returns the (shared) subtree of `map` at the `path`s drop it.length.
   * The tree is extended, if necessary, by adding additional nodes
   * for the characters along the given path.
   */
  private def prefixed(it: Iterator[CodePoint]): CodePointSeqMap[T] =
    if (!it.hasNext)
      this
    else {
      val first = it.next()
      suffixes get first match {
        case None =>
          suffixes = suffixes + (first -> empty)
        case _ =>
      }
      suffixes(first) prefixed (it)
    }

  /**
   * Returns the (shared) subtree of `map` at the path `s`.
   * The tree is extended, if necessary, by adding additional nodes
   * for the characters along the given path.
   */
  def prefixed(s: CodePointSeq): CodePointSeqMap[T] =
    prefixed(forwardIterator(s))

  var updated: Boolean = false

  /**
   * Post: map=map0 + s -> v
   */
  override def update(s: CodePointSeq, t: T) = {
    val path = prefixed(s)
    path.value = Some(t)
  }


  /**
   * Post: map=map0 + s.reverse -> v
   */
  def reverseUpdate(s: CodePointSeq, t: T) =
    prefixed(reversedIterator(s)).value = Some(t)

  /**
   * @see update
   * @return previous value
   */
  def change(s: CodePointSeq, t: T): Option[T] = {
    val path = prefixed(s)
    val result = path.value
    path.value = Some(t)
    result
  }

  /**
   * @see reverseUpdate
   * @return previous value
   */
  def reverseChange(s: CodePointSeq, t: T): Option[T] = {
    val path = prefixed(reversedIterator(s))
    val result = path.value
    path.value = Some(t)
    result
  }



  /**
   * Post:   map = map0 \ {s}
   * Return: if s in dom(map0) then Some(map0(s)) else None
   */
  override def remove(s: CodePointSeq): Option[T] = remove(s, 0)

  private def remove(s: CodePointSeq, from: Int): Option[T] =
    if (from == s.length) {
      val prev = value
      value = None
      prev
    }
    else suffixes get (s(from)) flatMap (_.remove(s, from + 1))

  /**
   * Return an iterator that yields the pairs of the maplet.
   */
  def iterator: Iterator[(CodePointSeq, T)] =
    (for {v <- value.iterator} yield (Nil, v)) ++ // the value, if any
      (for {(chr: CodePoint, map) <- suffixes // the suffix mappings
            (s, v) <- map // results from the suffix mappings
            }
      yield (chr +: s, v))

  /** An iterator that returns the pairs of the mapping, with
   * domain elements represented as lists of characters.
   */
  def pathIterator: Iterator[(CodePointSeq, T)] =
    (for {v <- value.iterator} yield (Nil, v)) ++ // the value, if any
      (for {(chr, map) <- suffixes // the suffix mappings
            (s, v) <- map.pathIterator // results from the suffix mappings
            }
      yield (chr +: s, v))

  def paths: Iterator[CodePointSeq] =
    for {(path, _) <- pathIterator} yield path


  /**
   * Augment the mapping with `pair`,  and return the mapping itself.
   */
  def addOne(pair: (CodePointSeq, T)): this.type = {
    update(pair._1, pair._2); this
  }

  /**
   * Diminish the mapping by removing `s` from its domain, and return the mapping itself.
   */
  def subtractOne(s: CodePointSeq): this.type = {
    remove(s); this
  }

  /** A string representing the
   * structure of the representation
   * of the mapping
   */
  def show: String = repString(1)

  private def repString(indent: Int): String = {
    val b = new StringBuilder
    if (value.nonEmpty) {
      b append " -> "
      b append (value.get.toString)
    }
    b append ('\n')
    for {(ch, tree) <- suffixes} {
      b append ("  " * indent)
      b append (ch.toChar)
      b append (tree.repString(indent + 1))
    }
    b.toString
  }

  @inline override def empty = new CodePointSeqMap[T]
}

object CodePointSeqMap {
  type CodePoint = Int
  type CodePointSeq = Seq[CodePoint]

  @inline def forwardIterator(s: CodePointSeq): Iterator[CodePoint] = s.iterator

  /**  Reversed iterator over an unchanging `s` [EFFICIENT] */
  def reversedIterator(s: CodePointSeq): Iterator[CodePoint] = new Iterator[CodePoint] {
    var i: Int = s.length
    def hasNext: Boolean = i > 0
    def next(): CodePoint = if (i > 0) {
      i -= 1; s(i)
    } else 0
  }

  /**  Reversed iterator over an unchanging `s[0 until upto]` [EFFICIENT]  */
  def reversedIterator(s: CodePointSeq, upTo: Int): Iterator[CodePoint] = new Iterator[CodePoint] {
    var i: Int = upTo
    def hasNext: Boolean = i > 0
    def next(): CodePoint = if (i > 0) {
      i -= 1; s(i)
    } else 0
  }
}

