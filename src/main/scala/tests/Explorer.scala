package org.sufrin.glyph
package tests

import files.{FileAttributes, Folder, Shelf}
import styled.{CheckBox, Label, RadioCheckBoxes}
import tests.PathProperties._
import Modifiers.Bitmap
import cached._
import files.FileAttributes.Row
import gesture.{Gesture, Keystroke}
import NaturalSize.Grid
import styles.decoration.{Framed, RoundFramed}

import io.github.humbleui.jwm.Key
import org.sufrin.logging.{FINEST, SourceLoggable}

import java.awt.Desktop
import java.nio.file.{FileSystems, NotDirectoryException, Path, Paths}
import scala.collection.mutable





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
        List("Id",
             "Name",
             "Size", "Size+", "Perms",
             "#",
             "Owner", "Group", "Created",
             "Modified", "Accessed")

    /** The list of potential layouts (in order of presentation) */
    val fieldLayouts: Seq[FieldLayout] =
        List(rightJustify(8)(_),
             leftJustify(FileAttributes.rowNameLength)(_),
             rightJustify(10)(_),  rightJustify(10)(_), rightJustify(10)(_),
             centerJustify(2)(_),
             rightJustify(10)(_),  rightJustify(10)(_), centerJustify(21)(_),
             centerJustify(21)(_), centerJustify(21)(_))

    /** The mapping from potential fields to their layouts  */
    val fieldLayout: Map[String, FieldLayout] =
      collection.immutable.ListMap.from(allFields zip fieldLayouts)

    /** The set of fields currently selected for displaying */
    val displayed: mutable.Set[String] = mutable.Set[String]("Name", "Size+", "Modified", "Owner", "Permissions", "Accessed")

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
          case "Size+"    => layout(row.compSize)
          case "Size"     => layout(row.attributes.size.toString)
          case "Perms"    => layout(row.attributes.mode)
          case "Owner"    => layout(row.attributes.owner.toString) //.leftJustify(folder.ownerWidth)
          case "Group"    => layout(row.attributes.group.toString) // .leftJustify(folder.groupWidth)
          case "Modified" => (timeOf(row.attributes.lastModifiedTime).toString)
          case "Created"  => (timeOf(row.attributes.creationTime()).toString)
          case "Accessed" => (timeOf(row.attributes.lastAccessTime()).toString)
          case "Id"       => layout(row.inum)
          case "#"        => layout(row.links)
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

  object Actions {


    def delete(): Unit   = delete(Shelf.paths)
    def delete(paths: Seq[Path]): Unit = {
      println(s"Delete: ${paths.mkString(" ")}")
    }



    def print(): Unit = view.selectedRows.length match {
      case 1 =>
        val path = theRows(view.selectedRows(0)).path
        if (path.isDir) view.bell.play() else Explorer.printOrdinaryFile(path)
      case _ =>
        view.bell.play()
    }

    def currentImplicitDestination: Option[Path] = view.selectedRows.length match {
      case 0                            => Some(folder.path)
      case 1 if theRows(0).isDirectory  => Some(theRows(view.selectedRows.head).path)
      case _  =>
        view.bell.play()
        None
    }

    def currentImplicitDestinationString: String = currentImplicitDestination match {
      case None => "(destination is ambiguous)"
      case Some(path) => path.toString
    }

    val trashPath = folder.path.resolve(".trash-explorer")

    /** selected paths to the shelf */
    def clearShelf(): Unit = Shelf.clear()
    def shelf(): Unit  = shelf(view.selectedRows.map { i => theRows(i).path })
    def shelf(paths: Seq[Path]): Unit = {
      println(s"Shelf =  ${paths.mkString(" ")}")
      Shelf.clear()
      Shelf.add(paths)
      view.clearSelection()
    }
    def unShelf(): Unit  = unShelf(view.selectedRows.map { i => theRows(i).path })
    def unShelf(paths: Seq[Path]): Unit = {
      println(s"Shelf -=  ${paths.mkString(" ")}")
      Shelf.remove(paths)
      view.clearSelection()
    }

    // implicit source: the clipboard/shelf
    def copy(path: Path): Unit = {
      println(s"Copy: ${Shelf.paths.mkString(" ")} to ${path}")
      popupErrors(FileOperations.copy(Shelf.paths, path))
    }
    def copy(): Unit = currentImplicitDestination.foreach(copy)


    def popupErrors(errors: Seq[Exception]): Unit =
      if (errors.nonEmpty) styled.windowdialogues.Dialogue.OK(Label(errors.mkString("\n"))).InFront(GUI).start()


    // implicit source: the shelf
    def move(path: Path): Unit = {
      println(s"Move: ${Shelf.paths.mkString(" ")}  to ${path}")
      popupErrors(FileOperations.move(Shelf.paths, path))
      Shelf.clear()
    }
    def move(): Unit = currentImplicitDestination.foreach(move)

    // implicit source: the shelf
    def link(path: Path): Unit = {
      println(s"Link: ${Shelf.paths.mkString(" ")}  to ${path}")
      popupErrors(FileOperations.link(Shelf.paths, path))
      Shelf.clear()
    }
    def link(): Unit = currentImplicitDestination.foreach(link)


    lazy val shelfButton = TextButton("(S)=", hint = Hint(2, "Add selection to (s)helf")) {
      _ => shelf()
    }

    lazy val unShelfButton = TextButton("(S)-=", hint = Hint(2, "Remove selection from (s)helf")) {
      _ => unShelf()
    }

    lazy val clearButton = TextButton("(S)={}", hint = Hint(2, "Clear (s)helf")) {
      _ => clearShelf()
    }

    lazy val shelfButtons = NaturalSize.Row(shelfButton, unShelfButton, clearButton)

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption()), constant = false, preferredLocation = Some(Vec(15,15)))

    lazy val deleteButton = TextButton("del", hint=shelfHint(()=>"Delete files")){
      _ => delete()
    }

    lazy val copyButton = TextButton("cp", hint=shelfHint(()=>s"Copy files to ${currentImplicitDestinationString}")){
      _ => copy()
    }

    lazy val moveButton = TextButton("mv", hint=shelfHint(()=>s"Move files to ${currentImplicitDestinationString}")){
      _ => move()
    }

    lazy val linkButton = TextButton("ln", hint=shelfHint(()=>s"Link files to ${currentImplicitDestinationString}")){
      _ => link()
    }


    lazy val GUI = NaturalSize.Row(align=Mid)(shelfButtons, fileSheet.hSpace(3), copyButton, moveButton, linkButton, deleteButton)

  }

  lazy val view: unstyled.dynamic.SeqViewer = new unstyled.dynamic.SeqViewer(
                                columns, 30,
                                fileSheet.buttonFont,
                                fileSheet.buttonForegroundBrush,
                                fileSheet.buttonBackgroundBrush,
                                Brushes.red,
                                theListing,
                                autoScale=true,
                                header=List(theHeader.value)) {

    override def isUnderlined(i: Int): Boolean = Shelf.contains(theRows(i).path)

    override def onClick(mods: Bitmap, selected: Int): Unit = Explorer.open((folder.path.resolve(theRows(selected).path)))

    override def onHover(mods: Bitmap, hovered: Int): Unit = if (false) folder.withValidCaches {
      if (theRows.isDefinedAt(hovered)) {
        val path = theRows(hovered).path.toString
        println(path)
      }
    }

    override def onOther(gesture: Gesture): Unit = {
      import GlyphTypes.Key

      import gesture._
      gesture match {
        case Keystroke(Key.W, _) if PRESSED =>
          folder.close()
          view.guiRoot.close()

        case Keystroke(Key.C, _) if PRESSED =>
          Actions.shelf()

        case Keystroke(Key.X, _) if PRESSED =>
          Actions.delete()

        case Keystroke(Key.V, _) if PRESSED =>
          Actions.copy()

        case Keystroke(Key.P, _) if PRESSED =>
          Actions.print()

        case other =>
          println(other)
          bell.play()
      }
    }
  }

  val invisibilityCheckBox =
    styled.SymbolicCheckBox(showingInvisibles, whenTrue="(.)", whenFalse="()", hint=Hint(2, "Show\ninvisible\nfiles")){
      state =>
      showingInvisibles = state
      theListing.clear()
      view.refresh(theListing, reset=true)
  }

  val reverseSortCheckBox =
    styled.SymbolicCheckBox(reverseSort, whenTrue="↓", whenFalse="↑", hint=Hint(2, "Reverse\nsort\norder")){
      state =>
        reverseSort = state
        theListing.clear()
        view.refresh(theListing, reset=true)
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
  }

  lazy val sortButtons: RadioCheckBoxes = RadioCheckBoxes(
    captions = List("Name", "Size", "Created", "Modified", "Accessed"),
    prefer   = "Name"
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
    val hintSheet: StyleSheet = fileSheet.copy(fontScale=0.7f)
    val buttonSheet: StyleSheet = fileSheet.copy(fontScale=0.8f)
    private lazy val prefixDirs = folder.prefixPaths.toSeq.scanLeft(Path.of("/"))((b, p) => b.resolve(p))

    private lazy val buttons: Seq[Glyph] = prefixDirs.tail.map {
      case path =>
        TextButton(s"${path.getFileName.toString}", hint=Hint(3, path.toRealPath().toString, preferredLocation = Some(Vec(10, 10)))(hintSheet))
        { _ => Explorer.open(path) }(buttonSheet)
    }.prepended(TextButton(s"/") { _ => Explorer.open(Path.of("/")) }(buttonSheet))

    lazy val parent = {
      val up = folder.path.resolve("..").toRealPath()
      TextButton(s"..", hint=Hint(3, up.toString, preferredLocation = Some(Vec(10, 10)))(hintSheet))
      { _ => Explorer.open(up) }(buttonSheet)
    }

    lazy val path: Glyph  = NaturalSize.Row(StateButtons.buttons)
    lazy val order: Glyph = NaturalSize.Row(align=Mid)(List(Label("Ordered"), reverseSortCheckBox, Label("on" )) ++ sortButtons.glyphButtons(Right, fixedWidth = false) ++ List(invisibilityCheckBox))
  }

  lazy val displayStyleGUI = NaturalSize.Col(
    FixedSize.Row(width=view.w, align=Mid)(fileSheet.hFill(), Fields.selectors, fileSheet.hFill()) above
    FixedSize.Row(width=view.w, align=Mid)(fileSheet.hFill(), StateButtons.order, fileSheet.hFill())).enlarged(15).roundFramed(radius=20)

  lazy val pathGUI =
    FixedSize.Row(width=view.w, align=Mid)(StateButtons.path, fileSheet.hSpace(), reValidateButton, fileSheet.hFill(), StateButtons.parent).enlarged(15).roundFramed(radius=20)


  lazy val GUI: Glyph =
    NaturalSize.Col()(
      FixedSize.Row(width=view.w, align=Mid)(Actions.GUI, fileSheet.hFill(), helpButton),

      view.enlarged(15),

      pathGUI,
      fileSheet.vSpace(1),
      displayStyleGUI,
    ).enlarged(15, fg=Brushes.lightGrey)
}

object Explorer extends Application with SourceLoggable {
  level = FINEST

  import GlyphTypes.FontStyle.NORMAL

  implicit val fileSheet: StyleSheet =
    StyleSheet(buttonDecoration=Framed(), buttonFontSize = 18, labelFontSize = 18, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.blue)

  val root = new Folder(FileSystems.getDefault.getPath("/Users/sufrin"))

  lazy val GUI: Glyph = new Explorer(root)(fileSheet).GUI

  def title: String = s"Explore: ${root.path.abbreviatedString()}"

  override val dock = Dock("Exp\nlore", bg=Brushes.yellow)

  val dialogueSheet = fileSheet.copy(buttonForegroundBrush=Brushes.red, buttonDecoration=RoundFramed(Brushes.redFrame, radius=10))

  def dialogueLabel(message: String): Glyph = Label(message)(dialogueSheet).roundFramed(fg=Brushes.red(width=10), radius = 10f).enlarged(10)

  def openOrdinaryFile(path: Path): Unit =  {
    import java.awt.Desktop
    if (Desktop.isDesktopSupported) {
      val desktop = Desktop.getDesktop
      if (desktop.isSupported(Desktop.Action.OPEN))
        try desktop.open(path.toFile) // Opens with default application
        catch {
          case ex: java.io.IOException =>
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to open\n${path.toString}"))(dialogueSheet).InFront(GUI).start(floating = false)
        }
      else
        styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop open not supported")).InFront(GUI).start()
    }
    else
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop not supported on this platform")).InFront(GUI).start()
  }

  def printOrdinaryFile(path: Path): Unit =  {
    import java.awt.Desktop
    if (path.isReadable && !path.isDir) {
      if (path.isReadable && !path.isDir && Desktop.isDesktopSupported) {
      val desktop = Desktop.getDesktop
      if (desktop.isSupported(Desktop.Action.BROWSE))
        try desktop.browse(path.toUri) // print
        catch {
          case ex: java.io.IOException =>
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to brows (for printing)\n${path.toRealPath()}"))(dialogueSheet).InFront(GUI).start(floating = false)
        }
      else
        styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop print not supported")).InFront(GUI).start()
    }
    else
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop not supported on this platform")).InFront(GUI).start()
    } else {
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Unreadable, or folder\n$path"))(dialogueSheet).InFront(GUI).start()
    }
  }


  def open(path: Path): Unit =
    if (path.isReadable) {
      if (path.isDir) {
        try {
          val folder = new Folder(path)
          styled.windowdialogues.Dialogue.FLASH(new Explorer(folder).GUI, title = folder.path.abbreviatedString()).OnRootOf(GUI).start(floating = false)
        } catch {
          case ex: NotDirectoryException => styled.windowdialogues.Dialogue.OK(Label(s"Not a directory\n${ex.getMessage}")).OnRootOf(GUI).start()
        }
      } else
        openOrdinaryFile(path)
    } else
    if (path.isDir) { // Apparently non-readable: may still have ACL permissions as a directory
      val desktop = Desktop.getDesktop
      if (desktop.isSupported(Desktop.Action.OPEN))
        try desktop.open(path.toFile) // Opens with platform default
        catch {
          case ex: java.io.IOException =>
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to open\n${path.toRealPath()}"))(dialogueSheet).InFront(GUI).start(floating = false)
        }
    } else {
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Unreadable\n$path"))(dialogueSheet).InFront(GUI).start()
    }

  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
  }

  lazy val homePath = Paths.get(System.getProperty("user.home"))
}



