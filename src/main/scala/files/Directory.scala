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


  import org.sufrin.microCSO._

  def streamAll(    root: Path,
                    depth: Int = Int.MaxValue,
                    followSymbolic: Boolean = false,
                    includeRoot: Boolean = false,
                    out: OutPort[(Path, BasicFileAttributes)]
               ): proc = {
    proc(s"streamAll(out=${out.toString})") {
      def output(path: Path, attrs: BasicFileAttributes): Unit = out!(path, attrs)
      forAll(root, depth, followSymbolic, includeRoot, output)
      out.closeOut()
    }
  }

  def forMatches(
      root: Path,
      pattern: PathMatcher,
      depth: Int = Int.MaxValue,
      followSymbolic: Boolean = false,
      includeRoot: Boolean = false,
      out: (Path, BasicFileAttributes) => Unit,
      warn: String => Unit = { _ => () },
  ): Unit = {
    def output(path: Path, attrs: BasicFileAttributes): Unit = if ((pattern eq null) || pattern.matches(path)) {
      out(path, attrs)
      val depth = path.getNameCount
      fine(s"${"  " * depth} ${path.getFileName}")
    }
    forAll(root, depth, followSymbolic, includeRoot, output)
  }

  def forAll(root: Path, depth: Int = Int.MaxValue, followSymbolic: Boolean = false, includeRoot: Boolean = false, out: (Path, BasicFileAttributes) => Unit): Unit =
  { val exclude: Path = if (includeRoot) null else root
    Files.walkFileTree(
      root,
      if (followSymbolic) FOLLOW else NOFOLLOW,
      depth,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            path: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
            out(path, attrs)
            val depth = path.getNameCount
            fine(s"${"  " * depth} ${path.getFileName}")
          FileVisitResult.CONTINUE
          }

        override def preVisitDirectory(
            path: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if (path != exclude) out(path, attrs)
          val depth = path.getNameCount
          fine(s"${"  " * depth} ${path.getFileName}")
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(
            path: Path,
            exn: IOException
        ): FileVisitResult = {
          fine(exn.toString)
          FileVisitResult.CONTINUE
        }
      }
    )
  }
}
