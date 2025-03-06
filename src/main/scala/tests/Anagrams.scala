package org.sufrin.glyph
package tests

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}


class Anagrams {
  type Bucket = ArrayBuffer[String]
  val dict = new LinkedHashMap[String, Bucket]()
  var maxLength: Int = 0

  def size: Int = dict.size

  def enter(word: String): Unit = {
    val anagrams = dict.getOrElseUpdate(word.sorted, new Bucket)
    anagrams.append(word)
    maxLength  = maxLength max word.length
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
  object byWordLength extends Ordering[Bucket] {
    def compare(x: Bucket, y: Bucket): Int = x.head.size-y.head.size
  }

  lazy val longestWord: String = dict.keys.max(byLength)

  lazy val longestProper: Seq[String] = dict.values.filter{ set => set.length>1 }.max(byWordLength).toSeq

  lazy val most: Seq[String] = dict.values.max(byCount).toSeq

  type Barrel = ArrayBuffer[Bucket]

  lazy val countIndexed: Array[Barrel] = {
    val buckets: Array[Barrel] = Array.ofDim[Barrel](maxLength)
    for { i<-0 until buckets.length} buckets(i)=null
    for { bucket <- dict.values } {
      val bucketLength = bucket.length
      if (buckets(bucketLength) eq null) buckets(bucketLength) = new Barrel
      buckets(bucketLength).append(bucket)
    }
    buckets
  }

  lazy val lengthIndexed: Array[Barrel] = {
    val buckets: Array[Barrel] = Array.ofDim[Barrel](maxLength+1)
    for { i<-0 until buckets.length} buckets(i)=null
    for { key <- dict.keys } {
      val wordLength = key.length
      val bucket = dict(key)
      if (bucket.length>1) {
        if (buckets(wordLength) eq null) buckets(wordLength) = new Barrel
        buckets(wordLength).append(bucket)
      }
    }
    buckets
  }

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


    if (args.contains("-byword"))
    for { (k, s) <- anagrams.dict }
      if (s.size>1)
        println(s"${s.mkString(" ")}")

    if (args.contains("-bycount")) {
      val ix = anagrams.countIndexed
      for {i <- 0 until ix.length if (ix(i) ne null) && ix(i).head.length > 1} {
        for {bucket <- ix(i)} println(s"$i: ${bucket.mkString(" ")}")
      }
    }

    if (args.contains("-bywordlength"))
    {
      val ix = anagrams.lengthIndexed
      for {i <- 0 until ix.length if (ix(i) ne null)} {
        for {bucket <- ix(i)} println(s"$i: ${bucket.mkString(" ")}")
      }
    }

    { println("Longest words with anagrams")
      val ix = anagrams.lengthIndexed
      var longest = ix.length-1
      while (longest>=0 && (ix(longest) eq null)) longest -= 1
      for { bucket <- ix(longest) } println(s"$longest: ${bucket.mkString(" ")}")
    }

    { println("Words with most anagrams")
      val ix = anagrams.countIndexed
      var longest = ix.length-1
      while (longest>=0 && (ix(longest) eq null)) longest -= 1
      for { bucket <- ix(longest) } println(s"$longest: ${bucket.mkString(" ")}")
    }

  }
}
