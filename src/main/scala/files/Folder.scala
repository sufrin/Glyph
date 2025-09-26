package org.sufrin.glyph
package files

import org.sufrin.glyph.cached.Cached
import FileAttributes.Row
import org.sufrin.logging.SourceLoggable

import java.nio.file.{LinkOption, NoSuchFileException, Path}
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.Files.readAttributes
import java.util.Comparator
import scala.collection.mutable
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IteratorHasAsScala}

object Folder extends SourceLoggable {
  def apply(path: Path): Folder = new Folder(path)

  def readPosixAttributes(path: Path): PosixFileAttributes =
    try { readAttributes(path, classOf[PosixFileAttributes]) }
    catch { case ex: NoSuchFileException => UndefinedAttributes() }

  def readImmediatePosixAttributes(path: Path): PosixFileAttributes =
    try {
      readAttributes(
        path,
        classOf[PosixFileAttributes],
        LinkOption.NOFOLLOW_LINKS
      )
    } catch { case ex: NoSuchFileException => UndefinedAttributes() }

}

/**  Proxy for the directory/folder at `path`.
  */
class Folder(val path: Path) {
  import Folder._

  val absolutePath: Path = path.toAbsolutePath.toRealPath()

  import java.nio.file.Files.list
  private val byName = new Comparator[Path] {
    def compare(o1: Path, o2: Path): Int =
      o1.toString.compareToIgnoreCase(o2.toString)
  }

  var groupWidth, ownerWidth, nameWidth: Int = 0

  lazy val sortedRows: Cached[Seq[Row]] = Cached[Seq[Row]] {
    import FileAttributes._
    require(path.toFile.isDirectory, s"$path is not a directory")
    val stream = list(path).sorted(byName)
    groupWidth = 0
    ownerWidth = 0
    nameWidth = 0
    stream.iterator().asScala.toSeq.map { case path: Path =>
      val row = Row(path, readPosixAttributes(path))
      val (owidth, gwidth, nwidth) = row.principalWidths
      if (owidth > ownerWidth) ownerWidth = owidth
      if (gwidth > groupWidth) groupWidth = owidth
      if (nwidth > nameWidth) nameWidth = nwidth
      row
    }
  }

  lazy val entryCount: Cached[Long] = Cached[Long] {
    list(path).count()
  }

  lazy val rowIndex: Cached[mutable.Map[Path, Row]] =
    Cached[mutable.Map[Path, Row]] {
      val rows = sortedRows.value
      val map = mutable.LinkedHashMap[Path, Row]()
      for { row <- rows } map(row.path) = row
      map
    }

  def sortedDirs: Seq[Row] = sortedRows.value.filter(_.isDirectory)
  def sortedFiles: Seq[Row] = sortedRows.value.filterNot(_.isDirectory)

  lazy val prefixPaths: Seq[Path] = {
    val p = path.normalize()
    val count = p.getNameCount
    for { i <- 0 until count } yield p.getName(i)
  }

  def reValidate(): Unit = {
    sortedRows.clear()
    sortedRows.value
    rowIndex.clear()
    entryCount.clear()
    rowIndex.value
    ()
  }

  /** Invalidate cache and its dependents
    */
  def invalidate(): Unit = {
    for { cache <- List(sortedRows, rowIndex, entryCount) } cache.clear()
    finest(s"invalidated primary caches for: $path")
  }

  def withValidCaches[T](expression: => T): T = {
    validate()
    expression
  }

  private var _onValidate: String => Unit = (_ => ())
  def onValidate(response: String => Unit): Unit = _onValidate = response

  /**  When /anything/ happens to this directory we clear the relevant caches.
    *  This crude approach was once refined, but the cost of the complexity
    *  outweighed the saving in resources -- at least for reasonably-sized
    *  folders.
    */
  private val watching = java.nio.file.FileSystems.getDefault.newWatchService()
  locally {
    import java.nio.file.StandardWatchEventKinds._
    path.register(watching, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW)
  }

  def close(): Unit = {
    watching.close()
  }

  def validate(): Unit = {
    var lastEvent: String = ""
    val watchKey = watching.poll
    if (watchKey ne null) {
      var changes = false
      for { event <- watchKey.pollEvents.asScala } {
        val changedKey = event.context.asInstanceOf[Path]
        val changedPath = path.resolve(changedKey)
        lastEvent = (s"${event.kind} ${event.context} $changedPath.$lastEvent")
        changes = true
      }
      watchKey.reset()
      if (changes) {
        invalidate()
        _onValidate(lastEvent)
      }
    }
  }
}
