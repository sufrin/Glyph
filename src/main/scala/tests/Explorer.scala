package org.sufrin.glyph
package tests

import files.Folder
import styled.{CheckBox, Label, RadioCheckBoxes}
import tests.PathProperties._
import Modifiers.Bitmap
import cached._
import files.FileAttributes.Row
import gesture.{Gesture, Keystroke}
import NaturalSize.Grid
import styles.decoration.RoundFramed

import io.github.humbleui.jwm.Key
import org.sufrin.logging.{FINEST, SourceLoggable}

import java.awt.datatransfer.FlavorEvent
import java.io.File
import java.nio.file.{FileSystems, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

object FileOperations {
  import java.nio.file.{StandardCopyOption, _}

  def copy(from: Path, to: Path): Seq[Exception] = {
    // pre: to is a writeable folder
    if (from.isReadable) {
      try {
        Files.copy(from, to.resolve(from.getFileName), StandardCopyOption.COPY_ATTRIBUTES)
        Seq.empty
      }
      catch { case ex: java.io.IOException => List(ex) }
    }
    else List(new java.nio.file.NoSuchFileException(from.toString, "", " file is unreadable."))
  }

  def copy(from: Seq[Path], to: Path): Seq[Exception] = {
    if (to.isDir) from.flatMap{ case path => copy(path, to) } else Seq.empty
  }
}



class Explorer(folder: Folder)(implicit val fileSheet: StyleSheet)  {
  import styled.TextButton

  var showingInvisibles = false
  var reverseSort = false

  locally {
    folder.onValidate{
      case event: String =>
        Explorer.finest(s"about to refresh after: $event")
        view.refresh(theListing)
        Explorer.finest(s"refreshed after: $event")
    }
  }

  object Orderings {
    import org.sufrin.glyph.files.FileAttributes._
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
    def bySize(rows: Seq[Row]): Seq[Row]    = rows.sorted(bySize)
    def byName(rows: Seq[Row]): Seq[Row]    = rows

    def reverseView[T](s: Seq[T]): Seq[T] = new Seq[T] { host =>

      def apply(i: Int): T = s(length-i-1)

      val length: Int = s.length

      def iterator: Iterator[T] = new Iterator[T] {
        var i: Int = host.length
        def hasNext: Boolean = i>0
        def next(): T = { i-=1; s(i) }
      }
    }
  }

  var theOrdering: Seq[Row]=>Seq[Row] = Orderings.byName

  def theRows: Seq[Row] = {
    val sort: Seq[Row]=>Seq[Row] = if (reverseSort) theOrdering.andThen(Orderings.reverseView(_)) else theOrdering
    val dirs  =  sort(folder.sortedDirs)
    val files =  sort(folder.sortedFiles)
    val rows = if (showingInvisibles) (dirs ++ files) else (dirs++files).filterNot(_.isHidden)
    rows
  }

  /**
   * `selectors` is a glyph composed of (suitably-captioned) checkboxes that control whether a field is to be
   * presented (or not) in the current folder listing.
   */
  object Fields {
    import org.sufrin.utility.CharSequenceOperations._
    type FieldLayout = CharSequence => CharSequence
    /** The list of potential fields (in order of presentation) */
    val allFields: Seq[String] =
        List("Name", "Size", "Size(xb)", "Perms", "Owner", "Group", "Created", "Modified", "Accessed")

    /** The list of potential layouts (in order of presentation) */
    val fieldLayouts: Seq[FieldLayout] =
        List(leftJustify(34)(_), rightJustify(10)(_), rightJustify(10)(_), rightJustify(10)(_), rightJustify(10)(_), rightJustify(10)(_), centerJustify(20)(_), centerJustify(20)(_), centerJustify(20)(_))

    /** The mapping from potential fields to their layouts  */
    val fieldLayout: Map[String, FieldLayout] =
      collection.immutable.ListMap.from(allFields zip fieldLayouts)

    /** The set of fields currently selected for displaying */
    val displayed: mutable.Set[String] = mutable.Set[String]("Name", "Size(xb)", "Modified", "Owner", "Permissions", "Accessed")

    /** apply `action`  to each field selected for display */
    def forDisplayedFields(action: String => Unit): Unit = {
      for {disp <- allFields if displayed.contains(disp)} action(disp)
    }

    lazy val selectors: Glyph = {
      val labels: Seq[Glyph] = allFields.map(label => Label(label))
      val toggles: Seq[Glyph] = allFields.map { label =>
        CheckBox(displayed.contains(label)) {
          case false => displayed.remove(label); view.refresh(theListing)
          case true  => displayed.add(label); view.refresh(theListing)
        }
      }
      Grid(width = labels.length, padx = 10).table(labels ++ toggles)
    }

    /* Some field widths are determined per-folder */
    def layout(fieldName: String): FieldLayout =
      fieldName match {
        case "Owner" => centerJustify(folder.ownerWidth max 5)(_)
        case "Group" => centerJustify(folder.groupWidth max 5)(_)
        case _       => fieldLayout(fieldName)
      }
  }

  def theHeader: Cached[String] = Cached[String]{
    import org.sufrin.glyph.files.FileAttributes._
    import org.sufrin.utility.CharSequenceOperations._
    val out: StringBuilder = new  StringBuilder()
    Fields.forDisplayedFields
    { case field: String =>
           out.append(Fields.layout(field)(field))
           out.append(" ")
    }
    out.toString
  }

  def showRow(row: Row): String = {
    import org.sufrin.glyph.files.FileAttributes._
    import org.sufrin.utility.CharSequenceOperations._
    val out = new StringBuilder()
    Fields.forDisplayedFields
     {  case field: String =>
        val layout = Fields.layout(field)
        val text: CharSequence = field match {
          case "Name"     => layout(s"${row.dirToken} ${row.name}${row.linkToken}")
          case "Size(xb)" => layout(row.attributes.bkmg)
          case "Size"     => layout(row.attributes.size.toString)
          case "Perms"    => layout(row.attributes.mode)
          case "Owner"    => layout(row.attributes.owner.toString) //.leftJustify(folder.ownerWidth)
          case "Group"    => layout(row.attributes.group.toString) // .leftJustify(folder.groupWidth)
          case "Modified" => timeOf(row.attributes.lastModifiedTime).toString
          case "Created"  => timeOf(row.attributes.creationTime()).toString
          case "Accessed" => timeOf(row.attributes.lastAccessTime()).toString
          case _ => ""
        }
        out.append(text)
        out.append(" ")
    }
    out.toString
  }

  def theListing: CachedSeq[Row, String] = folder.withValidCaches {
    CachedSeq(theRows) (showRow)
  }

  lazy val columns = 90

  lazy val view = new unstyled.dynamic.SeqViewer(
                                columns, 30,
                                fileSheet.buttonFont,
                                fileSheet.buttonForegroundBrush,
                                fileSheet.buttonBackgroundBrush,
                                Brushes.red,
                                theListing,
                                autoScale=true,
                                header=List(theHeader.value))
  {

    override def onClick(mods: Bitmap, selected: Int): Unit = Explorer.open((folder.path.resolve(theRows(selected).path)))

    override def onHover(mods: Bitmap, hovered: Int): Unit = folder.withValidCaches {}

    override def onOther(gesture: Gesture): Unit = {
      import GlyphTypes.Key

      import gesture._
      gesture match {
        case Keystroke(Key.W, _) if PRESSED =>
          folder.close ()
          GUI.guiRoot.close ()

        case Keystroke(Key.C, _) if PRESSED =>
          import ClipboardHelper._
          val selected = selectedRows.map{ i=> folder.path.resolve(theRows(i).path).toFile }
          val selectedList = new java.util.ArrayList[File](selected.length)
          for { f <- selected } selectedList.add(f)
          println(selected)
          putFiles(selectedList)

        case Keystroke(Key.V, _) if PRESSED =>
          import ClipboardHelper._
          val selected = getFiles().asScala.toSeq.map{ case file => Path.of(file.getPath) }
          println(FileOperations.copy(selected, folder.path))


        case other =>
          bell.play()
      }
    }

  }

  val invisibilityCheckBox =
    styled.SymbolicCheckBox(showingInvisibles, whenTrue="(.)", whenFalse="()", hint=Hint(2, "Show\ninvisible\nfiles")){
    state =>
      showingInvisibles = state
      theListing.clear()
      view.refresh(theListing)
  }

  val reverseSortCheckBox =
    styled.SymbolicCheckBox(reverseSort, whenTrue="↓", whenFalse="↑", hint=Hint(2, "Reverse\nsort\norder")){
      state =>
        reverseSort = state
        theListing.clear()
        view.refresh(theListing)
    }.scaled(1.5f)


  val helpButton = styled.TextButton("Help") { _ => }
  locally {
    helpButton.enabled(false)
  }

  /** Force a revalidation and redisplay: should not normally be needed */
  val reValidateButton = TextButton("\u21BB", hint=Hint(2, "Force revalidation of the display\n(not usually needed)")){
    state =>
      folder.reValidate()
      theListing.clear()
      view.refresh(theListing)
  }.scaled(1.7f)

  lazy val sortButtons: RadioCheckBoxes = RadioCheckBoxes(
    captions = List("N", "S", "C", "M", "A"),
    prefer   = "N"
    )
    { case selected => selected match
        {
          case Some(0) => theOrdering = Orderings.byName
          case Some(1) => theOrdering = Orderings.bySize
          case Some(2) => theOrdering = Orderings.byCreateTime
          case Some(3) => theOrdering = Orderings.byModTime
          case Some(4) => theOrdering = Orderings.byAccessTime
          case _       => sortButtons.select(0); theOrdering = Orderings.byName
        }
        theListing.clear()
        view.refresh(theListing)
  }

  /**
   * `path` is a sequence of buttons; each corresponding to an ancestor of this
   * folder in the filestore, and labelled with its filename (not its path). Pressing a button
   * opens an explorer on the ancestor.
   *
   * `order` is a sequence of (mostly) captioned toggles to control the order of presentation of the rows
   */
  object StateButtons {
    private lazy val prefixDirs = folder.prefixPaths.toSeq.scanLeft(Path.of("/"))((b, p) => b.resolve(p))
    private lazy val dotdot = {
      val up = folder.path.resolve("..").toRealPath()
      TextButton(s"/..", hint=Hint(3, up.toString)) { _ => Explorer.open(up) }
    }
    private lazy val buttons: Seq[Glyph] = prefixDirs.tail.map {
      case path => TextButton(s"/${path.getFileName.toString}", hint=Hint(3, path.toRealPath().toString)) { _ => Explorer.open(path) }.framed()
    }.prepended(TextButton(s"/") { _ => Explorer.open(Path.of("/")) }.framed())
      .appended(dotdot.framed())

    lazy val path: Glyph  = NaturalSize.Row(StateButtons.buttons)
    lazy val order: Glyph = NaturalSize.Row(align=Mid)(List(Label("Ordered"), reverseSortCheckBox, Label("on" )) ++ sortButtons.glyphButtons(Right, fixedWidth = false) ++ List( invisibilityCheckBox, helpButton))
  }

  val clipboardFG: Brush = Brushes.lightGrey
  val clipboardAlert = unstyled.Label("Clipboard", fileSheet.buttonFont, clipboardFG)
  locally {
    ClipboardWatcher{
      clipboardFG.color(Brushes.red.color)
    }
  }

  lazy val GUI: Glyph =
    NaturalSize.Col()(
      FixedSize.Row(width=view.w, align=Mid)(StateButtons.path, fileSheet.hSpace(), reValidateButton, clipboardAlert, fileSheet.hFill(), StateButtons.order).enlarged(15).roundFramed(radius=20),
      view.enlarged(15),
      FixedSize.Row(width=view.w, align=Mid)(fileSheet.hFill(), Fields.selectors, fileSheet.hFill()).enlarged(15).roundFramed(radius=20),
      ).enlarged(15, fg=Brushes.lightGrey)
}

object Explorer extends Application with SourceLoggable {
  level = FINEST

  import GlyphTypes.FontStyle.NORMAL

  implicit val fileSheet: StyleSheet =
    StyleSheet(buttonFontSize = 18, labelFontSize = 18, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.blue)

  val root = new Folder(FileSystems.getDefault.getPath("/Users/sufrin"))

  lazy val GUI: Glyph = new Explorer(root)(fileSheet).GUI

  def title: String = "Explore"

  override val dock = Dock("Exp\nlore", bg=Brushes.yellow)

  val dialogueSheet = fileSheet.copy(buttonForegroundBrush=Brushes.red, buttonDecoration=RoundFramed(Brushes.redFrame, radius=10))

  def dialogueLabel(message: String): Glyph = Label(message)(dialogueSheet).roundFramed(fg=Brushes.red(width=10), radius = 10f).enlarged(10)

  def open(path: Path): Unit =
    if (path.isReadable) {
      if (path.isDir) {
        val folder = new Folder(path)
        styled.windowdialogues.Dialogue.FLASH(new Explorer(folder).GUI, title=folder.path.toString).OnRootOf(GUI).start(floating=false)
      } else {
        import java.awt.Desktop
        if (Desktop.isDesktopSupported) {
          val desktop = Desktop.getDesktop
          if (desktop.isSupported(Desktop.Action.OPEN))
            try desktop.open(path.toFile) // Opens with default application
            catch {
              case ex: java.io.IOException =>
                styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to open\n${path.toRealPath()}"))(dialogueSheet).InFront(GUI).start(floating = false)
            }
          else
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop open not supported")).InFront(GUI).start()
        }
        else
          styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop not supported on this platform")).InFront(GUI).start()
      }
    } else {
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Unreadable\n$path"))(dialogueSheet).InFront(GUI).start()
    }

  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
  }
}

object PathProperties {
  implicit class PathProperty(val path: Path) extends AnyVal {
    def isHidden: Boolean  = path.getFileName.toString.charAt(0)=='.'//java.nio.file.Files.isHidden(path)
    def isVisible: Boolean = !isHidden
    def isDir: Boolean     = java.nio.file.Files.isDirectory(path)
    def toFile: File       = path.toFile
    def isReadable: Boolean= path.toFile.canRead
  }
}

