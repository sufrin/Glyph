package org.sufrin.glyph
package files

import asynchronous._
import cached.Cached
import files.FileAttributes.Row
import notifier.Notifier

import org.sufrin.logging.{SourceDefault, SourceLoggable}

import java.nio.file.{LinkOption, NoSuchFileException, Path}
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.Files.readAttributes
import java.util.Comparator
import scala.collection.mutable
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IteratorHasAsScala}

object Folder extends SourceLoggable {

  def apply(path: Path): Folder = {
    val folder =
    cache.get(path) match {
      case None =>
        val f = new Folder(path)
        cache(path) = f
        refCount(path) = 1
        f
      case Some(f) =>
        refCount(path) += 1
        f
    }
    Folder.finest(s"Folder cache for $path referenced ${refCount(path)}")
    folder
  }

  def remove(path: Path): Unit = {
    assert(refCount.contains(path), s"Cannot remove folder for $path")
    refCount(path) -= 1
    if (refCount(path)<=0) {
      refCount.remove(path)
      cache.remove(path)
      Folder.finest(s"Folder cache for $path removed")
    } else {
      Folder.finest(s"Folder cache for $path released by one client (referenced ${refCount(path)})")
    }
  }

  def withFolderFor(path: Path)(fn: Folder => Unit): Unit = {
    cache.get(path) match {
      case None         =>
        SourceDefault.warn(s"No folder for $path")
        println(refCount)
        println(cache)
      case Some(folder) => fn(folder)
    }
  }

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

 private val cache: mutable.LinkedHashMap[Path,Folder] = new mutable.LinkedHashMap[Path,Folder]
 private val refCount: mutable.LinkedHashMap[Path,Int] = new mutable.LinkedHashMap[Path,Int]

}

/**  Proxy for the directory/folder at `path`.
  */
class Folder(val path: Path) {
  import Folder._

  locally { println(s"Folder($path)") }
  val folderChanged: Notifier[String] = new Notifier[String]{}

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

  def sortedDirs: Seq[Row]  = sortedRows.value.filter(_.isDirectory)
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

  def notifyChange(): Unit = {
    invalidate()
    folderChanged.notify("Direct notification")
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
    Folder.remove(path)
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
        folderChanged.notify(lastEvent)
      }
    }
  }
}
