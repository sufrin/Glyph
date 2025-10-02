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
import overlaydialogues.Dialogue
import styles.decoration.RoundFramed

import io.github.humbleui.jwm.Key
import org.sufrin.logging.{FINEST, SourceLoggable}

import java.awt.Desktop
import java.nio.file._
import scala.collection.mutable





class Explorer(folder: Folder)(implicit val fileSheet: StyleSheet)  { thisExplorer =>
  import styled.TextButton

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


  object Actions {


    def delete(): Unit   = delete(Shelf.paths)
    def delete(paths: Seq[Path]): Unit = {
      println(s"Delete: ${paths.mkString(" ")}")
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
      shelf(view.selectedRows.map { i => theRows(i).path })
      Shelf.forCut = forCut
    }

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
    def pasteTo(path: Path): Seq[Exception] = {
      println(s"Paste: ${Shelf.paths.mkString(" ")} to ${path}")
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
      println(s"Move: ${Shelf.paths.mkString(" ")}  to ${path}")
      popupErrors(FileOperations.move(Shelf.paths, path))
      Shelf.clear()
      //folder.withValidCaches { view.reDraw() }  // Notification now does this
    }

    def move(): Unit = currentImplicitDestination.foreach(move)

    // implicit source: the shelf
    // implicit source: the shelf
    def link(path: Path): Unit = {
      println(s"Link: ${Shelf.paths.mkString(" ")}  to ${path}")
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
      Explorer.open((folder.path.resolve(theRows(selected).path)))
      clearSelection()
    }

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
          thisExplorer.close()

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
          println(other)
          bell.play()
      }
    }
  }

  def close(): Unit = {
    folder.folderChanged.removeTagged(thisExplorer)
    folder.close()
    GUI.guiRoot.close()
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
        { _ => Explorer.open(path) }(buttonSheet)
    }.prepended(TextButton(s"/") { _ => Explorer.open(Path.of("/")) }(buttonSheet))

    lazy val parent = {
      val up = folder.path.resolve("..").toRealPath()
      TextButton(s"..", hint=Hint(3, up.toString, preferredLocation = Some(Vec(10, 10)))(hintSheet))
      { _ => Explorer.open(up) }(buttonSheet)
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
      val closeSettings = unstyled.reactive.RawButton(PolygonLibrary.closeButtonGlyph.scaled(2), down=Brushes.red, hover=Brushes.green) { _ => settingsPopup.close() }
      NaturalSize.Col(align = Left)(
        closeSettings,
        FixedSize.Row(width = view.w, align = Mid)(fileSheet.hFill(), Fields.selectors, fileSheet.hFill()),
        FixedSize.Row(width = view.w, align = Mid)(fileSheet.hFill(), order, fileSheet.hFill()), fileSheet.vSpace()
        )
    }

    private lazy val settingsPopup: Dialogue[Unit] = Dialogue.POPUP(settingsGUI, Seq.empty, None).AtTop(GUI)

    lazy val popupButton: Glyph = unstyled.reactive.RawButton(Label("Ξ").scaled(2f), down=Brushes.red, hover=Brushes.green) {
      _ =>
        settingsPopup.isModal = false
        settingsPopup.canEscape = true
        settingsPopup.start()
    }
  }

  lazy val pathGUI =
    FixedSize.Row(width=view.w, align=Mid)(StateButtons.path, fileSheet.hFill(), StateButtons.parent).enlarged(15).roundFramed(radius=20)


  lazy val GUI: Glyph =
    NaturalSize.Col()(
      FixedSize.Row(width=view.w, align=Mid)(Settings.popupButton, fileSheet.hSpace(), Actions.GUI, fileSheet.hFill()),
      fileSheet.vSpace(1),
      pathGUI,
      view.enlarged(15),
      ).enlarged(15, fg=Brushes.lightGrey)
}

object Explorer extends Application with SourceLoggable {
  level = FINEST

  import GlyphTypes.FontStyle.NORMAL

  implicit val fileSheet: StyleSheet =
    StyleSheet(buttonDecoration=RoundFramed(bg=Brushes.lightGrey, enlarge=9f, radius=10), buttonFontSize = 18, labelFontSize = 18, buttonFontStyle=NORMAL, labelForegroundBrush= Brushes.blue)

  val root = Folder(FileSystems.getDefault.getPath("/Users/sufrin"))

  val icon: Glyph = External.Image.readResource("/explorer.png").scaled(0.3f) // ExplorerIcon.icon.scaled(0.2f) // External.Image.read("/Users/sufrin/GitHomes/Glyph/ICONS/explorer.png").scaled(0.2f)

  lazy val GUI: Glyph = {
    lazy val helpDialogue: styled.windowdialogues.Dialogue[Unit] = {
      val helpSheet: StyleSheet = Explorer.fileSheet.copy(textFontSize = 16f, textForegroundBrush = Brushes.black)
      val translator = glyphML.Translator(helpSheet)
      import translator._
      val blurb: Glyph = <div width="80em" align="justify" parskip="0.5ex">
        <p align="center"><b>Display</b></p>
        <p>Each window corresponds to a directory in the filestore. Each of its files is shown on a single row in the display.
          The columns shown can be changed -- using the panel popped up by the <b>Ξ</b> button. So can the order in which they are
          shown. Showing "invisible" files is enabled by <b>[.]</b> and disabled by <b>(.)</b>.
        </p>
        <p>
          An <b>S</b> to the right of a date/time indicates
          daylight-saving (summer) time for the current locality on the given date.
        </p>
        <p>
          The <tt>size+</tt> column shows file sizes in compact human-readable form, namely a number followed by a suffix from <b>b k m g t</b> denoting
          its multiplication by a power of 10:  <b>0 3 6 9 12</b>. Directory sizes are given as their number of files (with suffix <b>f</b>).
        </p>
        <p>The path to the directory being viewed in the window is shown (above the listing) as a sequence of buttons, each of which corresponds to an
          ancestor and is associated with a "hover-hint" that shows the corresponding <i>real</i>
          path --  with symbolic links expanded when appropriate. Pressing any of them opens a fresh window on the ancestor.
        </p>
        <p align="center"><b>Selection</b></p>
        <p> Individual windows have a (possibly empty) selection: its rows are selected by mouse: </p>
        <scope>
          <attributes id="tag:p" hang=" *  " />
          <p> Primary mouse click: selects uniquely</p>
          <p> Primary mouse drag: extends the selection</p>
          <p> Shifted primary mouse click: extends the selection at one end</p>
          <p> Secondary mouse click (or C-Primary) (or drag) inverts selection status of the indicated row(s)</p>
          <p> Mouse double-click, or (ENTER), opens the file indicated by the selected row, if possible</p>
        </scope>
        <p align="center"><b>Shelf</b></p>
        <p> There is an Explorer-wide <b>Shelf</b> on which is a collection of paths that denote
          individual files in the filestore. A file is said to be "shelved" if its path is
          on the shelf. Shelved files are the objects of the actions describd below, and they are
          underlined in the display. If the underlining is "textured" then the corresponding file has been marked for deletion
          as part of the next "paste" (C-V).
        </p>

        <p>Actions are by buttons or keys.</p>
        <scope>
          <attributes id="tag:p" hang=" *  "  />
          <p> <b>Shelf</b> places the selection on the shelf.</p>
          <p> <b>C-C</b> places the selection on the shelf.</p>
          <p> <b>C-X</b> places the selection on the shelf, and marks it for later deletion as part of the next "paste" (C-V) action.</p>
          <p> <b>C-V</b> copies the shelved files to the folder in which it is clicked; deleting them if they were marked for deletion by (C-X).</p>
          <p> <b>del</b> deletes the shelved files.</p>
          <p> <b>cp mv ln</b> respectively copy, move, and link the shelved files to the
            current implicit destination. This is <i>either</i> the directory denoted by a single selected path in the window in which
            the button is pressed, <i>or if none is selected</i> the folder being shown in that window.
          </p>
          <p> <b>Clear</b> removes the deletion mark from the shelf if it is so marked; otherwise clears the shelf.</p>
          <p> The usual navigation methods: <b>home end page-up page-down scroll-wheel</b> can be used to scroll the view; and the <b>up down</b>
            keys add to the selection in the specified direction if shifted; otherwise selecting a single row in the specified direction.
          </p>
        </scope>
      </div>.enlarged(10)
      styled.windowdialogues.Dialogue.FLASH(blurb, title="Explorer Help")
    }

    lazy val helpButton: Glyph = styled.TextButton("Help")
    { _ =>
      if (helpDialogue.running.isEmpty) helpDialogue.OnRootOf(GUI).start(floating=false)
    }


    NaturalSize.Row(align=Mid)(
          icon,
          fileSheet.hFill(),
          styled.TextButton(s"$homePath"){ _ => openExplorerWindow(homePath) },
          fileSheet.hFill(),
          helpButton
      )
  }

  def title: String = s"Explore: ${root.path.abbreviatedString()}"

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

  def openExplorerWindow(path: Path): Unit = {
    val folder = Folder(path)
    val explorer = new Explorer(folder)
    val dialogue = styled.windowdialogues.Dialogue.FLASH(explorer.GUI, title = folder.path.abbreviatedString()).OnRootOf(GUI)
    dialogue.onClose { case _ =>
      folder.folderChanged.removeTagged(explorer)
      folder.close()
    }
    dialogue.start(floating = false)
    Application.enableAutoScaleFor(dialogue.GUI)
  }

  def open(path: Path): Unit = {
    if (path.isReadable) {
      if (path.isDir) {
        try {
          openExplorerWindow(path)
        } catch {
          case ex: NotDirectoryException => styled.windowdialogues.Dialogue.OK(Label(s"Not a directory\n${ex.getMessage}")).OnRootOf(GUI).start()
        }
      } else
        openOrdinaryFile(path)
    } else if (path.isDir) { // Apparently non-readable: may still have ACL permissions as a directory
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
  }


  override protected def whenStarted(): Unit = {
    super.whenStarted()
    GUI.guiRoot.autoScale=true
    Folder.level = FINEST
  }

  lazy val homePath = Paths.get(System.getProperty("user.home"))


}


object Icon {
  
}

