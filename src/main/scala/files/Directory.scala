package org.sufrin.glyph
package files

import org.sufrin.logging.SourceLoggable

import java.io.IOException

object Directory extends SourceLoggable {
  import java.nio.file._
  import java.nio.file.attribute.BasicFileAttributes
  import scala.collection.mutable.ListBuffer

  private lazy val NOFOLLOW = java.util.EnumSet.noneOf(classOf[FileVisitOption])
  private lazy val FOLLOW   = java.util.EnumSet.allOf(classOf[FileVisitOption])

  def allMatches[Result](
      root: Path,
      pattern: PathMatcher,
      depth: Int = Int.MaxValue,
      followSymbolic: Boolean = false
  )(result: Path => Result): Seq[Result] = {
    val results = ListBuffer.empty[Result]
    forMatches(
      root: Path,
      pattern: PathMatcher,
      depth: Int,
      followSymbolic: Boolean,
      { path => results += result(path) }
    )
    results.toSeq
  }

//  import org.sufrin.microCSO._
//
//  def streamMatches(
//                     root: Path,
//                     pattern: PathMatcher,
//                     depth: Int = Int.MaxValue,
//                     followSymbolic: Boolean = false,
//                     out: OutPort[Path]
//                   ): Unit = {
//
//  }

  def forMatches[Result](
      root: Path,
      pattern: PathMatcher,
      depth: Int = Int.MaxValue,
      followSymbolic: Boolean = false,
      out: Path => Unit
  ): Unit = {
    Files.walkFileTree(
      root,
      if (followSymbolic) FOLLOW else NOFOLLOW,
      depth,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            path: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if ((pattern eq null) || pattern.matches(path)) {
            out(path)
            val depth = path.getNameCount
            fine(s"${"  " * depth} ${path.getFileName}")
          }
          FileVisitResult.CONTINUE
        }

        override def preVisitDirectory(
            path: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if ((pattern eq null) || pattern.matches(path)) {
            out(path)
            val depth = path.getNameCount
            fine(s"${"  " * depth} ${path.getFileName}")
          }
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(
            path: Path,
            exn: IOException
        ): FileVisitResult = {
          warn(exn.toString)
          FileVisitResult.CONTINUE
        }
      }
    )
  }
}
