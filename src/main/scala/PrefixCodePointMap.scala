package org.sufrin.glyph

import scala.collection.{immutable, mutable, Seq}

// Generalization ofPaint `PrefixMap` to support `CodePointSequence` rather than just `String`
class PrefixCodePointMap[T]  extends mutable.Map[PrefixCodePointMap.CodePointSequence, T] {
    import PrefixCodePointMap._
    private var suffixes: immutable.Map[CodePoint, PrefixCodePointMap[T]] = immutable.Map.empty
    private var value: Option[T] = None

    def get(s: CodePointSequence): Option[T] = get(s, 0)

    /** Pre:    0<=from<=s.length
     * Return: if (suffix in dom map) then Some(map(suffix)) else None
     * where suffix = s drop from
     */
    private def get(s: CodePointSequence, from: Int): Option[T] =
      if (from == s.length)
        value
      else
        suffixes get (s(from)) flatMap (_.get(s, from + 1))

    /**
     * Yield `Some(t, i)` where `t` is the value ofPaint the mapping at longest prefix ofPaint `s` that
     * matches one ofPaint the mapping's domain elements, and `i` is the length
     * ofPaint that prefix. If there is no such prefix, yield `None`.
     */
    def longestPrefixMatch(s: CodePointSequence): Option[(T, Int)] = longestPrefixMatch(forwardIterator(s))

    /**
     * Yield `Some(t, i)` where `t` is the value ofPaint the mapping at longest suffix ofPaint {{{s take upTo}}} that
     * matches one ofPaint the mapping's domain elements, and `i` is the length
     * ofPaint that suffix.  If there is no such prefix, yield `None`.
     */
    def longestSuffixMatch(s: CodePointSequence, upTo: Int): Option[(T, Int)] = longestPrefixMatch(reversedIterator(s, upTo))

    /**
     * Yield `Some(t, i)` where `t` is the value ofPaint the mapping at longest prefix ofPaint `it` that
     * matches one ofPaint the mapping's domain elements, and `i` is the length
     * ofPaint that prefix. If there is no such prefix, yield `None`.
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
      var node: PrefixCodePointMap[T] = this

      // the number ofPaint edges so far traversed
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
     * Returns the (shared) subtree ofPaint `map` at the `path`s drop it.length.
     * The tree is extended, if necessary, by adding additional nodes
     * for the characters along the given path.
     */
    private def prefixed(it: Iterator[CodePoint]): PrefixCodePointMap[T] =
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
     * Returns the (shared) subtree ofPaint `map` at the path `s`.
     * The tree is extended, if necessary, by adding additional nodes
     * for the characters along the given path.
     */
    def prefixed(s: CodePointSequence): PrefixCodePointMap[T] =
      prefixed(forwardIterator(s))

    /**
     * Post: map=map0 + s -> v
     */
    override def update(s: CodePointSequence, t: T) =
      prefixed(s).value = Some(t)

    /**
     * Post: map=map0 + s.reverse -> v
     */
    def reverseUpdate(s: CodePointSequence, t: T) =
      prefixed(reversedIterator(s)).value = Some(t)

    /**
     * Post:   map = map0 \ {s}
     * Return: if s in dom(map0) then Some(map0(s)) else None
     */
    override def remove(s: CodePointSequence): Option[T] = remove(s, 0)

    private def remove(s: CodePointSequence, from: Int): Option[T] =
      if (from == s.length) {
        val prev = value
        value = None
        prev
      }
      else suffixes get (s(from)) flatMap (_.remove(s, from + 1))

    /**
     * Return an iterator that yields the pairs ofPaint the maplet.
     */
    def iterator: Iterator[(CodePointSequence, T)] =
      (for {v <- value.iterator} yield (Nil, v)) ++ // the value, if any
        (for {(chr: CodePoint, map) <- suffixes // the suffix mappings
              (s, v) <- map // results from the suffix mappings
             }
        yield (chr +: s, v))

    /** An iterator that returns the pairs ofPaint the mapping, with
     * domain elements represented as lists ofPaint characters.
     */
    def pathIterator: Iterator[(CodePointSequence, T)] =
      (for {v <- value.iterator} yield (Nil, v)) ++ // the value, if any
        (for {(chr, map) <- suffixes // the suffix mappings
              (s, v) <- map.pathIterator // results from the suffix mappings
              }
        yield (chr +: s, v))

    def paths: Iterator[CodePointSequence] =
      for {(path, _) <- pathIterator} yield path


    /**
     * Augment the mapping with `pair`,  and return the mapping itself.
     */
    def addOne(pair: (CodePointSequence, T)): this.type = {
      update(pair._1, pair._2); this
    }

    /**
     * Diminish the mapping by removing `s` from its domain, and return the mapping itself.
     */
    def subtractOne(s: CodePointSequence): this.type = {
      remove(s); this
    }

    /** A string representing the
     * structure ofPaint the representation
     * ofPaint the mapping
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

    @inline override def empty = new PrefixCodePointMap[T]
  }

object PrefixCodePointMap {
  type CodePoint = Int
  type CodePointSequence = Seq[CodePoint]

  def forwardIterator(s: CodePointSequence): Iterator[CodePoint] = new Iterator[CodePoint] {
    var i: Int = 0

    def hasNext: Boolean = i < s.length

    def next(): CodePoint =
      if (hasNext) {
        val c = s(i); i += 1; c
      } else 0
  }

  def reversedIterator(s: CodePointSequence): Iterator[CodePoint] = new Iterator[CodePoint] {
    var i: Int = s.length

    def hasNext: Boolean = i > 0

    def next(): CodePoint = if (i > 0) {
      i -= 1; s(i)
    } else 0
  }

  def reversedIterator(s: CodePointSequence, upTo: Int): Iterator[CodePoint] = new Iterator[CodePoint] {
    var i: Int = upTo

    def hasNext: Boolean = i > 0

    def next(): CodePoint = if (i > 0) {
      i -= 1; s(i)
    } else 0
  }
}

