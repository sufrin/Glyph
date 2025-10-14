package org.sufrin.glyph
package files
import java.nio.file._
import java.nio.file.attribute._
import scala.jdk.CollectionConverters._


object FileAttributes {
  import java.util.Calendar._
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

  val KB  = 1000d
  val MB  = 1000*KB
  val GB  = 1000*MB
  val TB  = 1000*GB
  implicit class legibleAttributes(attrs: PosixFileAttributes) {
    def id: String = attrs.fileKey().toString match {
      case s"(dev=$dev,ino=$ino)" => ino
      case id => id
    }
    def lastModifiedTime: FileTime = attrs.lastModifiedTime()
    def lastAccessTime: FileTime = attrs.lastAccessTime()
    def creationTime: FileTime = attrs.creationTime()
    def owner: UserPrincipal = attrs.owner()
    def group: GroupPrincipal = attrs.group()
    def permissions: collection.mutable.Set[PosixFilePermission] = attrs.permissions().asScala
    def mode: String = PosixFilePermissions.toString(attrs.permissions)
    def isDirectory: Boolean = attrs.isDirectory && !attrs.isOther
    def isSymbolicLink: Boolean = attrs.isSymbolicLink
    def size: Long = attrs.size
    def dsize: Double = size.toDouble
    def d = if (isDirectory) "\u25A2" else " "
    def s: String = if (isSymbolicLink) "@" else " "
    def dmode: String = s"$d$mode$s"
    def meaningfulSize: String = {
      if (isDirectory) "-    " else
        if (dsize<KB) s"${size}b" else
          if (dsize<MB) f"${dsize/KB}%1.1fk" else
            if (dsize<GB) f"${dsize/MB}%1.1fm" else
              if (dsize<TB) f"${dsize/GB}%1.2fg" else  f"${dsize/TB}%1.2ft"
    }

    def asString: String = s"$dmode $owner.$group C ${timeOf(creationTime)} M ${timeOf(lastModifiedTime)} A ${timeOf(lastAccessTime)} $meaningfulSize"
  }

  var rowNameLength:  Int     = 38
  val ellipsisString: String  = " \u22ef "
  val ellipsisLength: Int     = ellipsisString.length
  val suffixLength: Int       = 7

  case class Row(path: Path, var attributes: PosixFileAttributes) {
    val dirToken: String = attributes.d
    lazy val name = {
      val fname = path.getFileName
      val name: String = if (fname eq null) "" else fname.toString
      if (name.length < rowNameLength) name else {
        val prefix = name.take(rowNameLength-ellipsisLength-suffixLength-1)
        val suffix = name.takeRight(suffixLength) //drop(name.length - 7)
        s"$prefix${ellipsisString}$suffix"
      }
    }
    lazy val compSize: String  =
      try   { if (isDirectory) s"${Files.list(path).count}f" else attributes.meaningfulSize }
      catch { case ex: Exception => "??" }
    lazy val links:    String  = Files.getAttribute(path, "unix:nlink").toString
    lazy val inum:     String  = Files.getAttribute(path, "unix:ino").toString
    lazy val kind:     String = {
      val approx = Files.probeContentType(path)
      if (approx eq null) "" else approx
    }
    val isLink:        Boolean = Folder.readImmediatePosixAttributes(path).isSymbolicLink
    val linkToken:     String  = if (isLink) "@" else " "
    val isHidden:      Boolean = name.isEmpty||name(0)=='.'
    val isDirectory:   Boolean = attributes.isDirectory
    val isRegularFile: Boolean = attributes.isRegularFile
    val asString:      String  = f"$name%-30s ${attributes.asString}"
    def update():      Unit    = { attributes = Folder.readPosixAttributes(path) }
    def principalWidths:(Int,Int,Int) = (attributes.owner.toString.length, attributes.group.toString.length, name.length)

  }

  object Orderings {



    private val byModifiedTime = new Ordering[Row] {
      def compare(r1: Row, r2: Row): Int = r1.attributes.lastModifiedTime().compareTo(r2.attributes.lastModifiedTime())
    }
    private val byCreatedTime = new Ordering[Row] {
      def compare(r1: Row, r2: Row): Int = r1.attributes.creationTime().compareTo(r2.attributes.creationTime())
    }
    private val byAccessTime = new Ordering[Row] {
      def compare(r1: Row, r2: Row): Int = r1.attributes.lastAccessTime().compareTo(r2.attributes.lastAccessTime())
    }

    private val bySize = new Ordering[Row] {
      def compare(r1: Row, r2: Row): Int = r1.attributes.size().compareTo(r2.attributes.size())
    }

    def byModTime(rows: Seq[Row]): Seq[Row] = rows.sorted(byModifiedTime)

    def byAccessTime(rows: Seq[Row]): Seq[Row] = rows.sorted(byAccessTime)

    def byCreateTime(rows: Seq[Row]): Seq[Row] = rows.sorted(byCreatedTime)

    def bySize(rows: Seq[Row]): Seq[Row] = rows.sorted(bySize)

    def byName(rows: Seq[Row]): Seq[Row] = rows

    def reverseView[T](s: Seq[T]): Seq[T] = new Seq[T] {
      thisView =>
      def apply(i: Int): T = s(length - i - 1)
      val length: Int = s.length
      def iterator: Iterator[T] = new Iterator[T] {
        var i: Int = thisView.length
        def hasNext: Boolean = i > 0
        def next(): T = {
          i -= 1; s(i)
        }
      }
    }
  }


}


