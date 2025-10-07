package org.sufrin.glyph
package tests
package explorer

import files.FileAttributes

import java.io.File
import java.nio.file.Path

object PathProperties {
  implicit class PathProperty(val path: Path) extends AnyVal {
    def isHidden: Boolean = path.getFileName.toString.charAt(
      0
    ) == '.' //java.nio.file.Files.isHidden(path)
    def isVisible: Boolean = !isHidden
    def isDir: Boolean = java.nio.file.Files.isDirectory(path)
    def toFile: File = path.toFile
    def isReadable: Boolean = path.toFile.canRead
    def names: Seq[Path] = for { i <- 0 until nameCount } yield path.getName(i)
    def nameCount: Int = path.getNameCount
    def prefix: Path = {
      val p = path.getParent
      if (p eq null) path else p
    }

    /** Generate a (more) concise-looking "real" path by successively replacing the longest intermediate filename(s) in the path by "..."
      * and the user's home directory by "~". The path itself does not, in general, denote a file.
      */
    private def abbreviated(size: Int): Path = {
      val realPath = path.toRealPath()
      if (realPath.toString.length <= size) path
      else
        try {
          val home = Explorer.homePath
          val homePath = home.toRealPath()
          val fileName = realPath.getFileName
          val prefixSize = size - fileName.toString.length
          var currentPrefix = realPath.prefix
          def currentLength = currentPrefix.toString.length
          def largest: Path = currentPrefix.names.maxBy(_.toString.length)
          var rounds = 0
          //println(currentPrefix)
          while (
            rounds < 3 && currentLength > prefixSize && largest.toString.length > ellipsisLength
          ) {
            val shortNames: Seq[Path] = currentPrefix.names.map { path =>
              if (path == largest) ellipsis else path
            }
            rounds += 1
            //print(s"$rounds ${currentPrefix}")
            currentPrefix = Path.of("/").resolve(shortNames)
            //println(s" => ${currentPrefix}")
          }
          if (currentPrefix.startsWith(home))
            Path
              .of("~")
              .resolve(currentPrefix.names.drop(homePath.nameCount))
              .resolve(fileName)
          else
            currentPrefix.resolve(fileName)
        } catch {
          case ex: NullPointerException => path
        }
    }

    def resolve(paths: Seq[Path]): Path =
      paths.foldLeft(path)((p, name) => p.resolve(name))

    /** An abbreviated string rendering of this path made real, with "~" replacing the home directory, and some sort of ellipsis for
      * (sequences of) long intermediate directory names. The string does not necessarily represent a valid path.
      */
    def abbreviatedString(size: Int = 40): String = abbreviated(size).toString
      .replace(ellipses, shortEllipses)
      .replace(ellipses, shortEllipses)
  }

  val pathSep: Char = java.io.File.separatorChar
  val ellipsisString: String = FileAttributes.ellipsisString
  val ellipsis: Path = Path.of(ellipsisString)
  val ellipses: String = s"$ellipsisString$pathSep$ellipsisString"
  val shortEllipses: String = s"$ellipsisString$ellipsisString"
  val ellipsisLength: Int = ellipsisString.length
}
