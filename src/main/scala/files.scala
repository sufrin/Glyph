package org.sufrin.glyph
package files
import cached._
import files.FileAttributes.Row

import org.sufrin.logging.SourceLoggable

import java.nio.file._
import java.nio.file.attribute._
import java.nio.file.Files.readAttributes
import java.util
import java.util.Comparator
import scala.collection.mutable
import scala.jdk.CollectionConverters._


object System {
  lazy val fileStores: Iterable[FileStore] = FileSystems.getDefault.getFileStores.asScala
  lazy val rootDirs:   Iterable[Path] = FileSystems.getDefault.getRootDirectories.asScala
  lazy val supported:  mutable.Set[String] = FileSystems.getDefault.supportedFileAttributeViews.asScala
}

/**
 * An invariant `PosixFileAttributes` element with no permissions, created, modified, accessed at the epoch, and
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

    val epoch =  FileTime.fromMillis(0)

    def lastModifiedTime(): FileTime = epoch

    def lastAccessTime(): FileTime = epoch

    def creationTime(): FileTime = epoch

    def isRegularFile: Boolean = false

    def isDirectory: Boolean = false

    def isSymbolicLink: Boolean = false

    def isOther: Boolean = true

    def size(): Long = if (sizeDebug) 600000000000L+40000000000000L else 0

    def fileKey(): AnyRef = Nil
  }
  def apply(): PosixFileAttributes = singleton
}

object Folder extends SourceLoggable {
  def apply(path: Path): Folder = new Folder(path)


  def readPosixAttributes(path: Path): PosixFileAttributes =
    try { readAttributes(path, classOf[PosixFileAttributes]) }
    catch
    { case ex: NoSuchFileException => UndefinedAttributes() }

  def readImmediatePosixAttributes(path: Path): PosixFileAttributes =
    try { readAttributes(path, classOf[PosixFileAttributes], LinkOption.NOFOLLOW_LINKS) }
    catch
    { case ex: NoSuchFileException => UndefinedAttributes() }

}
/**
 *  Proxy for the directory/folder at `path`.
 */
class Folder(val path: Path) {
  import Folder._

  val absolutePath: Path = path.toAbsolutePath.toRealPath()

  import java.nio.file.Files.{list, readAttributes}
  private val byName = new Comparator[Path] { def compare(o1: Path, o2: Path): Int = o1.toString.compareToIgnoreCase(o2.toString) }

  var groupWidth, ownerWidth, nameWidth: Int = 0

  lazy val sortedRows: Cached[Seq[Row]] = Cached[Seq[Row]]{
      import FileAttributes._
      require (path.toFile.isDirectory, s"$path is not a directory")
      val stream = list(path).sorted(byName)
      groupWidth=0
      ownerWidth=0
      nameWidth=0
      stream.iterator().asScala.toSeq.map {
        case path: Path =>
          val row = Row(path, readPosixAttributes(path))
          val (owidth, gwidth, nwidth) = row.principalWidths
          if (owidth>ownerWidth) ownerWidth=owidth
          if (gwidth>groupWidth) groupWidth=owidth
          if (nwidth>nameWidth) nameWidth=nwidth
          row
      }
  }


  lazy val rowIndex: Cached[mutable.Map[Path, Row]] =  Cached[mutable.Map[Path, Row]] {
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
    for { i<-0 until count} yield p.getName(i)
  }

  def reValidate(): Unit = {
    sortedRows.clear()
    sortedRows.value
    rowIndex.clear()
    rowIndex.value
    ()
  }

  /**
   * Invalidate cache and its dependents
   */
  def invalidate(): Unit = {
    for { cache <- List(sortedRows, rowIndex) } cache.clear()
    finest(s"invalidated primary caches for: $path")
  }

  def withValidCaches[T](expression: => T): T = {
    validate()
    expression
  }

  private var _onValidate: String=>Unit = (_ =>())
  def onValidate(response: String=>Unit): Unit = _onValidate=response


  /**
   *  When /anything/ happens to this directory we clear the relevant caches.
   *  This crude approach was once refined, but the cost of the complexity
   *  outweighed the saving in resources -- at least for reasonably-sized
   *  folders.
   */
  private val watching = java.nio.file.FileSystems.getDefault.newWatchService()
  locally {
    import StandardWatchEventKinds._
    path.register(watching,ENTRY_CREATE,ENTRY_DELETE,ENTRY_MODIFY,OVERFLOW)
  }

  def close(): Unit = {
    watching.close()
  }

  def validate(): Unit = {
    var lastEvent: String = ""
    val watchKey = watching.poll
    if (watchKey ne null){
      var changes = false
      for { event <- watchKey.pollEvents.asScala } {
        import StandardWatchEventKinds._
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
    lazy val zoneoffset=if (dstOffset==0) "" else if (dstOffset>0) s"(${shorten(zoneID)}+$dstOffset)" else s"(${shorten(zoneID)}-${-dstOffset})"
    val offset = dstOffset match {
      case 0 => " "
      case 1 => "S" // "summer"/"saving"
      case _ => "?"
    }
    override val toString: String = f"$year%4d-$month%02d-$day%02d $h%02d:$m%02d:$s%02d $offset"
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
  val TB  = 1000*GB
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
    def size: Long = attrs.size
    def dsize: Double = size.toDouble
    def d = if (isDirectory) "\u25A2" else " "
    def s: String = if (isSymbolicLink) "@" else " "
    def dmode: String = s"$d$mode$s"
    def bkmg: String =
      if (dsize<KB) s"${size}b" else
        if (dsize<MB) f"${dsize/KB}%1.1fkb" else
          if (dsize<GB) f"${dsize/MB}%1.1fmb" else
            if (dsize<TB) f"${dsize/GB}%1.2fgb" else  f"${dsize/TB}%1.2ftb"
    def asString: String = s"$dmode $owner.$group C ${timeOf(creationTime)} M ${timeOf(lastModifiedTime)} A ${timeOf(lastAccessTime)} $bkmg"
  }

  case class Row(path: Path, var attributes: PosixFileAttributes) {
    val dirToken: String = attributes.d
    lazy val name = {
      val name = path.getFileName.toString
      if (name.length <= 30) name else {
        val prefix = name.take(20)
        val suffix = name.drop(name.length - 7)
        s"$prefix...$suffix"
      }
    }
    val isLink:        Boolean = Folder.readImmediatePosixAttributes(path).isSymbolicLink
    val linkToken: String = if (isLink) "@" else " "
    val isHidden:      Boolean = name(0)=='.'
    val isDirectory:   Boolean = attributes.isDirectory
    val isRegularFile: Boolean = attributes.isRegularFile
    val asString:      String  = f"$name%-30s ${attributes.asString}"
    def update():      Unit    = { attributes = Folder.readPosixAttributes(path) }
    def principalWidths:(Int,Int,Int) = (attributes.owner.toString.length, attributes.group.toString.length, name.length)

  }

}


