package org.sufrin.glyph
package tests

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}


class Anagrams {
  type Bucket = ArrayBuffer[String]
  val dict = new LinkedHashMap[String, Bucket]()

  def size: Int = dict.size

  def enter(word: String): Unit = {
    val anagrams = dict.getOrElseUpdate(word.sorted, new Bucket)
    anagrams.append(word)
  }

  def lookup(word: String): Seq[String] =
    dict.get(word.sorted) match {
      case Some(anagrams) => anagrams.toSeq
      case None           => Seq.empty
    }

  def apply(word: String): Seq[String] = lookup(word)

  object byLength extends Ordering[String] {
    def compare(x: String, y: String): Int = x.length-y.length
  }

  object byCount extends Ordering[Bucket] {
    def compare(x: Bucket, y: Bucket): Int = x.size-y.size
  }

  /** Relies on the data type invariant: compared buckets are nonempty, and
   *  contain strings of the same length.
   */
  object byKeyLength extends Ordering[Bucket] {
    def compare(x: Bucket, y: Bucket): Int = x.head.size-y.head.size
  }

  lazy val longestWord: String = dict.keys.max(byLength)

  lazy val longestProper: Seq[String] = dict.values.filter{ set => set.length>1 }.max(byKeyLength).toSeq

  lazy val most: Seq[String] = dict.values.max(byCount).toSeq


}

object Anagrams {
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("/usr/share/dict/words")
    val anagrams = new Anagrams
    var words = 0
    for { line <- source.getLines() } {
      anagrams.enter(line)
      words += 1
    }
    println(s"Words:            $words")
    println(s"Distinct words:   ${anagrams.size}")
    println(s"Longest word:     ${anagrams(anagrams.longestWord).mkString(" ")}")
    println(s"Longest anagram:  ${anagrams.longestProper.mkString(" ")}")
    println(s"Most anagrams:    ${anagrams.most.mkString(" ")}")
    if (args.contains("-all"))
    for { (k, s) <- anagrams.dict }
      if (s.size>1)
        println(s"${s.mkString(" ")}")
  }
}
