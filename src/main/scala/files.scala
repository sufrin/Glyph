package org.sufrin.glyph
package files
import cached._

import org.sufrin.logging.SourceLoggable

import java.nio.file._
import java.nio.file.attribute._
import java.util.Comparator
import scala.collection.mutable
import scala.jdk.CollectionConverters._


object System {
  lazy val fileStores: Iterable[FileStore] = FileSystems.getDefault.getFileStores.asScala
  lazy val rootDirs:   Iterable[Path] = FileSystems.getDefault.getRootDirectories.asScala
  lazy val supported:  mutable.Set[String] = FileSystems.getDefault.supportedFileAttributeViews.asScala
}


object Folder extends SourceLoggable {
  def apply(path: Path): Folder = new Folder(path)
}

/**
 *  Proxy for the directory/folder at `path`.
 */
class Folder(val path: Path) {
  import Folder._

  import java.nio.file.Files.{list, readAttributes}
  private val byName = new Comparator[Path] { def compare(o1: Path, o2: Path): Int = o1.toString.compareToIgnoreCase(o2.toString) }

  lazy val sortedPaths: Cached[Seq[Path]] = Cached[Seq[Path]]{
      require (path.toFile.isDirectory, s"$path is not a directory")
      (s"Re-reading $path")
      dirs.clear()
      notDirs.clear()
      attributeMap.clear()
      val stream = list(path).sorted(byName)
      stream.iterator().asScala.toSeq
  }

  lazy val dirs: Cached[Seq[Path]] = Cached {
    sortedPaths.value.filter(_.toFile.isDirectory)
  }

  lazy val notDirs: Cached[Seq[Path]] = Cached {
    sortedPaths.value.filterNot(_.toFile.isDirectory)
  }

  lazy val splitDirs: (Seq[Path], Seq[Path]) = (dirs.value, notDirs.value)

  def readPosixAttributes(path: Path): PosixFileAttributes = readAttributes(path, classOf[PosixFileAttributes], LinkOption.NOFOLLOW_LINKS)

  def attributes(paths: Seq[Path]): Seq[(Path, PosixFileAttributes)] = {
    paths.map { path => (path, readPosixAttributes(path))}
  }

  lazy val prefixPaths: Seq[Path] = {
    val count = path.getNameCount
    for { i<-0 until count} yield path.getName(i)
  }

  /**
   * A mapping from the filenames in this folder to their posix attributes. This
   * mapping is kept up-to-date incrementally whenever `validate()` is invoked.
   */
  lazy val attributeMap: Cached[mutable.LinkedHashMap[Path,PosixFileAttributes]] = Cached {
    val attrs = attributes(sortedPaths.value)
    val map = new mutable.LinkedHashMap[Path,PosixFileAttributes]
    for (entry <- attrs) map.addOne(entry)
    map
  }


  def reValidate(): Unit = {
    sortedPaths.clear()
    sortedPaths.value
    ()
  }

  /**
   * Invalidate cache and its dependents
   */
  def invalidate(): Unit = {
    for { cache <- List(sortedPaths, dirs, notDirs) } cache.clear()
    finest(s"invalidated primary caches for: $path")
  }

  def withValidCaches[T](expression: => T): T = {
    validate()
    expression
  }

  private var _onValidate: String=>Unit = (_ =>())
  def onValidate(response: String=>Unit): Unit = _onValidate=response

  /**
   *  When anything happens to this directory we clear all the (rarely-used)
   *  sequence caches as a whole; and we update the `attributeMap` incrementally.
   */
  private val watching = java.nio.file.FileSystems.getDefault.newWatchService()
  locally {
    import StandardWatchEventKinds._
    path.register(watching,ENTRY_CREATE,ENTRY_DELETE,ENTRY_MODIFY,OVERFLOW)
  }

  def validate(): Unit = {
    var lastEvent: String = ""
    val watchKey = watching.poll
    if (watchKey ne null) {
      var changes, overflow = false
      for { event <- watchKey.pollEvents.asScala } {
        import StandardWatchEventKinds._
        val changedKey = event.context.asInstanceOf[Path]
        val changedPath = path.resolve(changedKey)
        try {
          event.kind match {
            case ENTRY_CREATE =>
              attributeMap.value.update(changedKey, readPosixAttributes(changedPath))
            case ENTRY_MODIFY =>
              attributeMap.value.update(changedKey, readPosixAttributes(changedPath))
            case ENTRY_DELETE =>
              attributeMap.value.remove(changedKey)
            case OVERFLOW =>
              overflow = true
          }
          changes=true
          lastEvent=(s"${event.kind} ${event.context} $changedPath.$lastEvent")
        } catch {
          case exn: NoSuchFileException =>
          // a deletion has followed a create or a modify since the last
          // time validate was invoked so there are no attributes to read
          // from the filestore when we try to act on the preceding create/modify
          // events.
        }
      }
      watchKey.reset()
      if (changes) {
        invalidate()
        if (overflow) {
          attributeMap.clear()
          finest(s"invalidated attribute cache for: $path")
        }
        _onValidate(lastEvent)
      }
    }
  }

}

object FileAttributes {
  import java.time.Instant
  import java.util.Calendar._
  import java.util.Locale.{getDefault => locale}
  import java.util.TimeZone
  lazy val cal: java.util.Calendar = {
    val c = getInstance()
    c.setTimeZone(TimeZone.getDefault) // could be (eg) .getTimeZone("Europe/Paris"))
    c
  }

  def shorten(zoneID: String): String = {
    zoneID match {
      case "Europe/London" => "GMT"
      case s"$continent/$city" => city
      case other => other
    }
  }

  case class LegibleTime ( year: Int, month: Int, day: Int, h:Int, m:Int, s:Int, zoneID: String, zoneOffset: Int, dstOffset: Int) {
    val utcOffset = zoneOffset+dstOffset
    val offset=if (dstOffset==0) "" else if (dstOffset>0) s"(${shorten(zoneID)}+$dstOffset)" else s"(${shorten(zoneID)}-${-dstOffset})"
    override val toString = f"$year%4d-$month%02d-$day%02d $h%02d:$m%02d:$s%02d $offset"
  }

  def timeOf(time: FileTime): LegibleTime = {
    cal.setTimeInMillis(time.toMillis)
    LegibleTime(cal.get(YEAR), 1+cal.get(MONTH), cal.get(DAY_OF_MONTH), cal.get(HOUR_OF_DAY), cal.get(MINUTE), cal.get(SECOND),
                cal.getTimeZone.getID, cal.get(ZONE_OFFSET)/(1000*60*60), cal.get(DST_OFFSET)/(1000*60*60))
  }

  val CB  = 100d
  val KB  = 1000d
  val CKB = 100*1000d
  val MB  = 1000*1000d
  val CMB = 100*1000d
  val GB  = 1000*MB
  implicit class legibleAttributes(attrs: PosixFileAttributes) {
    def lastModifiedTime: FileTime = attrs.lastModifiedTime()
    def lastAccessTime: FileTime = attrs.lastAccessTime()
    def creationTime: FileTime = attrs.creationTime()
    def owner: UserPrincipal = attrs.owner()
    def group: GroupPrincipal = attrs.group()
    def permissions: collection.mutable.Set[PosixFilePermission] = attrs.permissions().asScala
    def mode: String = PosixFilePermissions.toString(attrs.permissions)
    def isDirectory: Boolean = attrs.isDirectory
    def isSymbolicLink: Boolean = attrs.isSymbolicLink
    val size: Long = attrs.size
    val dsize: Double = size.toDouble
    lazy private val d = if (isDirectory) "d" else "-"
    lazy private val s = if (isSymbolicLink) "@" else " "
    def dmode: String = s"$d$mode$s"
    val bkmg: String = if (dsize<KB) s"${size}b" else if (dsize<MB) f"${dsize/KB}%1.1fkb" else if (dsize<GB) f"${dsize/MB}%1.1fmb" else s"${dsize/GB}%1.2gb"
    def asString: String = s"$dmode $owner.$group C ${timeOf(creationTime)} M ${timeOf(lastModifiedTime)} A ${timeOf(lastAccessTime)} $bkmg"
  }

}

