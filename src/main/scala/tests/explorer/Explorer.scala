package org.sufrin.glyph
package tests
package explorer

import files.{FileAttributes, Folder, Shelf}
import styled._
import tests.explorer.PathProperties._
import Modifiers.Bitmap
import cached._
import files.FileAttributes.Row
import gesture.{Gesture, Keystroke}
import NaturalSize.Grid
import styled.overlaydialogues.Dialogue
import styles.decoration.{Framed, RoundFramed}

import io.github.humbleui.jwm.Key
import org.sufrin.logging.{FINEST, SourceLoggable}

import java.nio.file._
import scala.collection.mutable


/**
 * An `Explorer` implements the user interface to a `Folder`.
 * @param folder the `Folder` to be explored.
 * @param provider  provides opening and closing services to this explorer
 * @param fileSheet the stylesheet defining (much of) this explorer's appearance
 */
class Explorer(val folder: Folder, val provider: ExplorerServices)(implicit val fileSheet: StyleSheet)  { thisExplorer =>
  import styled.TextButton

  val serial = Explorer.nextSerial()

  override def toString: String = s"Explorer#$serial(${folder.path}, $provider)"

  var showingInvisibles = false
  var reverseSort = false

  // register this explorer with the folder
  locally {
    folder.folderChanged.handleWithTagged(thisExplorer) {
      case information: String =>
        Explorer.finest(s"(${folder.path}) refresh ing view after: $information")
        view.refresh(theListing)
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

    def reverseView[T](s: Seq[T]): Seq[T] = new Seq[T] { thisView =>

      def apply(i: Int): T = s(length-i-1)

      val length: Int = s.length

      def iterator: Iterator[T] = new Iterator[T] {
        var i: Int = thisView.length
        def hasNext: Boolean = i>0
        def next(): T = { i-=1; s(i) }
      }
    }
  }

  var theOrdering: Seq[Row]=>Seq[Row] = Orderings.byName

  def theRows: Seq[Row] = {
    val sort:   Seq[Row]=>Seq[Row] = if (reverseSort) theOrdering.andThen(Orderings.reverseView(_)) else theOrdering
    val filter: Seq[Row]=>Seq[Row] = if (showingInvisibles) { case rows:Seq[Row] => rows } else { case rows:Seq[Row] => rows.filterNot(_.isHidden) }
    val dirs  =  sort(filter(folder.sortedDirs))
    val files =  sort(filter(folder.sortedFiles))
    dirs++files
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
             "Kind",
             "Size", "Size+", "Perms",
             "#",
             "Owner", "Group", "Created",
             "Modified", "Accessed")

    /** The list of potential layouts (in order of presentation) */
    val fieldLayouts: Seq[FieldLayout] =
        List(rightJustify(8)(_),
             leftJustify(FileAttributes.rowNameLength+1)(_),
             leftJustify(12)(_),
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

  def columns: Int = theHeader.value.length

  def showRow(row: Row): String = {
    import org.sufrin.glyph.files.FileAttributes._
    import org.sufrin.utility.CharSequenceOperations._
    val out = new StringBuilder()
    Fields.forDisplayedFields
     {  case field: String =>
        val layout = Fields.layout(field)
        val text: CharSequence = field match {
          case "Name"     => layout(s"${row.dirToken} ${row.name}${row.linkToken}")
          case "Kind"     => layout(row.kind)
          case "Size+"    => layout(row.compSize)
          case "Size"     => layout(row.attributes.size.toString)
          case "Perms"    => layout(row.attributes.mode)
          case "Owner"    => layout(row.attributes.owner.toString)
          case "Group"    => layout(row.attributes.group.toString)
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

  def selectedPaths: Seq[Path] = view.selectedRows.map { i => theRows(i).path }

  object Actions {


    def delete(): Unit   = delete(Shelf.paths)
    def delete(paths: Seq[Path]): Unit = {
      Explorer.finer(s"Delete: ${paths.mkString(" ")}")
      popupErrors(paths.flatMap(FileOperations.delete(_)))
      //folder.withValidCaches { view.reDraw() }  // Notification now does this
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


    def shelf(forCut: Boolean): Unit  = {
      shelf(selectedPaths)
      Shelf.forCut = forCut
    }

    def shelf(paths: Seq[Path]): Unit = {
      Explorer.finer(s"Shelf =  ${paths.mkString(" ")}")
      Shelf.clear()
      Shelf.add(paths)
      view.clearSelection()
    }

    def unShelf(): Unit  = unShelf(selectedPaths)

    def unShelf(paths: Seq[Path]): Unit = {
      Explorer.finer(s"Shelf -=  ${paths.mkString(" ")}")
      Shelf.remove(paths)
      view.clearSelection()
    }

    // implicit source: the clipboard/shelf
    def pasteTo(path: Path): Seq[Exception] = {
      Explorer.finer(s"Paste: ${Shelf.paths.mkString(" ")} to ${path}")
      FileOperations.copy(Shelf.paths, path)
    }

    def paste(): Unit = {
      currentImplicitDestination match {
        case None =>
        case Some(destination) =>
          val errors = pasteTo(destination)
          if (errors.nonEmpty) popupErrors(errors)
          else
          if (Shelf.forCut) delete()
      }
      clearShelf()
      //folder.withValidCaches { view.reDraw() }  // Notification now does this
    }

    def popupErrors(errors: Seq[Exception]): Unit =
      if (errors.nonEmpty) styled.windowdialogues.Dialogue.OK(Label(errors.mkString("\n"))).InFront(GUI).start()


    // implicit source: the shelf
    def move(path: Path): Unit = {
      Explorer.finer(s"Move: ${Shelf.paths.mkString(" ")}  to ${path}")
      popupErrors(FileOperations.move(Shelf.paths, path))
      Shelf.clear()
      //folder.withValidCaches { view.reDraw() }  // Notification now does this
    }

    def move(): Unit = currentImplicitDestination.foreach(move)

    // implicit source: the shelf
    // implicit source: the shelf
    def link(path: Path): Unit = {
      Explorer.finer(s"Link: ${Shelf.paths.mkString(" ")}  to ${path}")
      popupErrors(FileOperations.link(Shelf.paths, path))
      Shelf.clear()
      //folder.withValidCaches { view.reDraw() } // Notification now does this
    }
    def link(): Unit = currentImplicitDestination.foreach(link)


    lazy val shelfButton = TextButton("Shelf", hint = Hint(2, "Selection to (s)helf\n... marked for copying (^C)\n... marked for deletion(^X)")) {
      _ => shelf(forCut = false)
    }

    lazy val clearButton = TextButton("Clear", hint = Hint(2, if (Shelf.nonEmpty && Shelf.forCut) "Clear shelf deletion mark" else "Clear shelf completely", constant = false)) {
      _ =>
        if(Shelf.nonEmpty && Shelf.forCut) Shelf.forCut = false else clearShelf()
        view.reDraw()
    }

    lazy val shelfButtons = NaturalSize.Row(shelfButton, clearButton)

    def shelfHint(caption: () => String): Hint = Hint.ofGlyph(4, Shelf.hintGlyph(caption()), constant = false, preferredLocation = Some(Vec(15,15)))

    lazy val deleteButton = TextButton("del", hint=shelfHint(()=>"Delete files")){
      _ => delete()
    }

    lazy val copyButton = TextButton("cp", hint=shelfHint(()=>s"Copy files to ${currentImplicitDestinationString} (C-V)")){
      _ => paste()
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
                                columns+1, 36,
                                fileSheet.buttonFont,
                                fileSheet.buttonForegroundBrush,
                                fileSheet.buttonBackgroundBrush,
                                Brushes.red,
                                theListing,
                                autoScale=true,
                                header=List(theHeader.value)) {

    override def isUnderlined(i: Int): Boolean = Shelf.contains(theRows(i).path)

    val cutBrush: Brush = fileSheet.buttonForegroundBrush.sliced(2,3f)

    override def underlineBrush: Brush = if (Shelf.forCut) cutBrush else fileSheet.buttonForegroundBrush

    override def onDoubleClick(mods: Bitmap, selected: Int): Unit = {
      val path = folder.path.resolve(theRows(selected).path)
      Explorer.finest(s"onDoubleClick(#$selected=$path) on $thisExplorer")
      provider.openPath(path)
      clearSelection()
    }

    override def onHover(mods: Bitmap, hovered: Int): Unit = if (false) folder.withValidCaches {
      if (theRows.isDefinedAt(hovered)) {
        val path = theRows(hovered).path.toString
        Explorer.finest(s"onHover(#$hovered=$path)")
        Explorer.finest(path)
      } else {
        Explorer.finest(s"onHover(#$hovered=???)")
      }
    }

    override def onOther(gesture: Gesture): Unit = {
      import GlyphTypes.Key

      import gesture._
      gesture match {

        case Keystroke(Key.C, _) if PRESSED =>
          Actions.shelf(forCut=false)

        case Keystroke(Key.X, _) if PRESSED =>
          Actions.shelf(forCut=true)

        case Keystroke(Key.BACKSPACE, _) if PRESSED =>
          Actions.delete()

        case Keystroke(Key.V, _) if PRESSED =>
          Actions.paste()

        case Keystroke(Key.P, _) if PRESSED =>
          Actions.print()

        case other =>
          Explorer.finest(s"onOther($gesture) on $thisExplorer")
          bell.play()
      }
    }
  }

  def close(): Unit = {
    folder.folderChanged.removeTagged(thisExplorer)
    folder.close()
    //GUI.guiRoot.close()
  }


  /** Force a revalidation and redisplay: should not normally be needed */
  val reValidateButton = TextButton("\u21BB", hint=Hint(2, "Force revalidation of the display\n(not usually needed)")){
    state =>
      folder.reValidate()
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
        { _ => provider.openPath(path) }(buttonSheet)
    }.prepended(TextButton(s"/") { _ => provider.openPath(Path.of("/")) }(buttonSheet))

    lazy val parent = {
      val up = folder.path.resolve("..").toRealPath()
      TextButton(s"..", hint=Hint(3, up.toString, preferredLocation = Some(Vec(10, 10)))(hintSheet))
      { _ => provider.openPath(up) }(buttonSheet)
    }

    lazy val path: Glyph  = NaturalSize.Row(buttons)

  }

  object Settings {
    val invisibilityCheckBox =
      styled.SymbolicCheckBox(showingInvisibles, whenTrue="(.)", whenFalse="[.]", hint=Hint(2, "Show\ninvisible\nfiles")){
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

    private lazy val order: Glyph =
      NaturalSize.Row(align=Mid)(List(Label("Ordered"), reverseSortCheckBox, Label("on" )) ++ sortButtons.glyphButtons(Right, fixedWidth = false) ++ List(invisibilityCheckBox))

    private val largeButtonStyle: StyleSheet = fileSheet.copy(buttonDecoration = styles.decoration.unDecorated, fontScale = 1.3f, buttonHoverBrush = fileSheet.buttonForegroundBrush)

    private lazy val settingsGUI = {
      val closeSettings = unstyled.reactive.RawButton(PolygonLibrary.closeButtonGlyph.scaled(1.5f), down=Brushes.red, hover=Brushes.green) { _ => settingsPopup.close() }
      NaturalSize.Col(align = Left)(
        closeSettings,
        FixedSize.Row(width = GUI.w, align = Mid)(fileSheet.hFill(), Fields.selectors, fileSheet.hFill()),
        FixedSize.Row(width = GUI.w, align = Mid)(fileSheet.hFill(), order, fileSheet.hFill()), fileSheet.vSpace()
        )
    }

    private lazy val settingsPopup: Dialogue[Unit] = overlaydialogues.Dialogue.POPUP(settingsGUI, Seq.empty, closeGlyph=None).AtTop(GUI)

    lazy val popupButton: Glyph = unstyled.reactive.RawButton(Label("Ξ").scaled(1.7f), down=Brushes.red, hover=Brushes.green) {
      _ =>
        settingsPopup.canEscape = true
        settingsPopup.start()
    }
  }


  lazy val pathGUI =
    FixedSize.Row(width=view.w, align=Mid)(StateButtons.path, fileSheet.hFill(), StateButtons.parent).enlarged(15).roundFramed(radius=20)

  def giveupFocus(): Unit = {
    if (GUI.hasGuiRoot) GUI.guiRoot.giveupFocus()

  }

  lazy val GUI: Glyph =
    NaturalSize.Col()(
      FixedSize.Row(width=view.w, align=Mid)(Settings.popupButton, fileSheet.hSpace(), Actions.GUI, fileSheet.hFill()),
      fileSheet.vSpace(1),
      pathGUI,
      view.enlarged(15),
      ).enlarged(15, fg=Brushes.lightGrey)
}


/**
 * The main application's root. Its GUI consistes of a single `ExplorerCollection` whose
 * initially selected `Explorer`  is bound to the current user's home directory.
 */
object Explorer extends Application with SourceLoggable {
  level = FINEST

  private var _serial = 0
  def nextSerial(): Int = { _serial +=1 ; _serial }

  import GlyphTypes.FontStyle.NORMAL

  implicit val fileSheet: StyleSheet =
    StyleSheet(buttonDecoration=RoundFramed(enlarge=9f, radius=0), buttonFontSize = 14, labelFontSize = 14, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.blue)

  val iconSheet = fileSheet.copy(buttonDecoration = Framed(Brushes.black(width=2)))

  val icon:     Glyph = External.Image.readResource("/explorer").scaled(0.03f)
  def menuIcon: Glyph = icon

  lazy val host = new ExplorerCollection(homePath)
  lazy val GUI: Glyph = host.GUI

  def title: String = s"Explore: ${homePath.abbreviatedString()}"
  override val dock: Dock = Dock(icon) // ("Exp\nlore", bg=Brushes.yellow)

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
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to browse (for printing)\n${path.toRealPath()}"))(dialogueSheet).InFront(GUI).start(floating = false)
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

  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
    Folder.level = FINEST
  }

  lazy val homePath = Paths.get(System.getProperty("user.home"))
}






