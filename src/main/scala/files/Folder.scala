package org.sufrin.glyph
package files

import asynchronous._
import cached.Cached
import files.FileAttributes.Row

import org.sufrin.logging.SourceLoggable
import org.sufrin.utility.Notifier

import java.nio.file._
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

    if (logging) finest(s"Folder cache for $path referenced ${refCount(path)}")
    if (logging) finest(toString)
    folder
  }

  def remove(path: Path): Unit = {
    assert(refCount.contains(path), s"Cannot remove folder for $path")
    refCount(path) -= 1
    if (logging) finest(s"Starting cache garbage collection (for $path) with $toString")
    for { path <- refCount.keys }
        if (refCount(path)<=0) {
          refCount.remove(path)
          cache.remove(path)
          Folder.finest(s"Folder cache for $path removed")
        }
    if (logging) finest(s"Finished cache garbage collection (for $path)")
  }

  /**
   *  If `path` denotes a cached `Folder` then `act` on that folder.
   *  Used only for notifications from `FileOperation`s.
   */
  def withFolderFor(path: Path)(act: Folder => Unit): Unit = {
    cache.get(path) match {
      case None          => if (logging) fine(s"$path is not a Directory")
      case Some(folder)  => act(folder)
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

 override def toString: String = {
   refCount.keys.toSeq.map { key => s"$key (${refCount(key)})"}.mkString("Cache:\n ", "\n ", "\n")
 }

}

/**  Proxy for the directory/folder at `path`.
  */
class Folder(val path: Path) {
  import Folder._

  val patternAssigned: Notifier[String] = new Notifier[String](s"patternAssigned:$path")

  /**
   *
   */
  def setPattern(source: String, tree: Boolean=false): Option[String] = {
    val err =
    if (source.isEmpty) {
      pattern = null
      findTree = false
      reValidate()
      None
    }
    else {
        try {
          pattern = FileSystems.getDefault.getPathMatcher(s"glob:**$source")
          findTree = tree
          reValidate()
          None
        } catch {
          case err: Throwable =>
            pattern = null
            findTree = false
            err.printStackTrace()
            reValidate()
            Some(err.getMessage)
        }
    }

    patternAssigned.notify(source)
    notifyChange()
    err
  }

  var findTree: Boolean = false
  var pattern: PathMatcher = null
  def matches(path: Path): Boolean = pattern==null || pattern.matches(path)

  /**
   * Subscription point for changes discovered (externally) in this folder
   */
  val folderChanged: Notifier[String] = new Notifier[String]{}

  /**
   * The real absolute path of this folder in the filestore. This
   * will not be the same as `path` if there are symbolic links
   * or "." or ".." references in `path`.
   */
  val absolutePath: Path = path.toAbsolutePath.toRealPath()

  import java.nio.file.Files
  private val byName = new Comparator[Path] {
    def compare(o1: Path, o2: Path): Int =
      o1.toString.compareToIgnoreCase(o2.toString)
  }

  /**
   * Widest group name, owner name, and filename in this folder.
   * Calculated as a side-effect of computing `allRowsByName`.
   */
  var groupWidth, ownerWidth, nameWidth: Int = 0


  /**
   * Rows (ordered by name) corresponding to the entries in this folder.
   * Invalidated when changes have been noticed in the folder.
   */
  private lazy val allRowsByName: Cached[Seq[Row]] = Cached[Seq[Row]] {
    import FileAttributes._
    require(path.toFile.isDirectory, s"$path is not a directory")
    groupWidth = 0
    ownerWidth = 0
    nameWidth = 0
    val mkRow: Path => Row = { case path: Path =>
      val row = Row(path, readPosixAttributes(path))
      val (owidth, gwidth, nwidth) = row.principalWidths
      if (owidth > ownerWidth) ownerWidth = owidth
      if (gwidth > groupWidth) groupWidth = owidth
      if (nwidth > nameWidth) nameWidth = nwidth
      row
    }

    if (findTree) {
      val seq = Directory.allMatches(path, pattern)(mkRow)
      seq
    }
    else {
      val stream = Files.list(path).filter(matches).sorted(byName)
      stream.iterator().asScala.toSeq.map(mkRow)
    }
  }

  /** Number of entries in the folder at this path */
  lazy val entryCount: Cached[Long] = Cached[Long] {
    Files.list(path).count()
  }

  def directoriesByName: Seq[Row] = allRowsByName.value.filter(_.isDirectory)
  def filesByName:       Seq[Row] = allRowsByName.value.filterNot(_.isDirectory)

  lazy val prefixPaths: Seq[Path] = {
    val p = path.normalize()
    val count = p.getNameCount
    for { i <- 0 until count } yield p.getName(i)
  }

  def reValidate(): Unit = {
    allRowsByName.clear()
    allRowsByName.value
    entryCount.clear()
    ()
  }

  def notifyChange(): Unit = {
    invalidate()
    folderChanged.notify("Direct notification")
  }

  /** Invalidate cache and its dependents
    */
  def invalidate(): Unit = {
    for { cache <- List(allRowsByName, entryCount)} cache.clear()
    if (logging) finest(s"invalidated primary caches for: $path")
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


