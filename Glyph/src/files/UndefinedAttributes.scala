package org.sufrin.glyph
package files

import java.nio.file.attribute._
import java.util

/** An invariant `PosixFileAttributes` element with no permissions, created, modified, accessed at the epoch, and
  * yielding true for `isOther`
  */
object UndefinedAttributes {
  private val sizeDebug = true
  private val singleton = new PosixFileAttributes {
    def owner(): UserPrincipal = new UserPrincipal {
      def getName: String = toString
      override val toString: String = "nobody"
    }

    def group(): GroupPrincipal = new GroupPrincipal {
      def getName: String = toString
      override val toString: String = "nogroup"

    }

    def permissions(): util.Set[PosixFilePermission] = util.Set.of()

    val epoch = FileTime.fromMillis(0)

    def lastModifiedTime(): FileTime = epoch

    def lastAccessTime(): FileTime = epoch

    def creationTime(): FileTime = epoch

    def isRegularFile: Boolean = false

    def isDirectory: Boolean = false

    def isSymbolicLink: Boolean = false

    def isOther: Boolean = true

    def size(): Long = if (sizeDebug) 600000000000L + 40000000000000L else 0

    def fileKey(): AnyRef = Nil
  }
  def apply(): PosixFileAttributes = singleton
}
