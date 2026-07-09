package org.sufrin.glyph
package tests
package barents

import files.{FileAttributes, Folder, Shelf}
import styled._
import tests.barents.PathProperties._
import Modifiers.Bitmap
import cached._
import files.FileAttributes.Row
import gesture.{Gesture, Keystroke}
import NaturalSize.Grid
import styled.overlaydialogues.Dialogue
import styles.decoration.RoundFramed
import unstyled.static.INVISIBLE
import Hint.{Centred, West}

import io.github.humbleui.jwm.Key
import org.sufrin.logging.{SourceDefault, SourceLoggable}

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.immutable.ListMap
import scala.collection.mutable


/**
 * A `Viewer` implements the user interface to a `Folder`.
 * @param folder the `Folder` to be explored.
 * @param services  provides opening and closing services to this viewer
 * @param fileSheet the stylesheet defining (much of) this viewer's appearance
 */
class Viewer(val folder: Folder, val services: ViewerServices)(implicit val fileSheet: StyleSheet) extends ActionProvider {
  thisViewer =>

  import styled.TextButton

  import Viewer.{finest, logging, warn}

  val serial = Viewer.nextSerial()

  override def toString: String = s"Viewer#$serial(${folder.path}, $services)"

  /** Include rows corresponding to "invisible" files. */
  var includeInvisible = false
  var reverseSort      = false

  // register this barents with the folder
  locally {
    folder.folderChanged.handleWithTagged(thisViewer) {
      case information: String =>
        if (logging) finest(s"(${folder.path}) refreshing viewer after: $information")
        viewer.refresh(theListing)
    }
  }

  def popupErrors(errors: Seq[Exception]): Unit =
    if (errors.nonEmpty)
      styled.windowdialogues.Dialogue
        .OK(Label(errors.mkString("\n")).roundFramed(Brushes.red(width=5), radius=5))
        .InFront(GUI)
        .start()

  import FileAttributes.Orderings




  /** The current viewer header */
  def theHeader: Cached[String] = Cached[String] {
    val out: StringBuilder = new StringBuilder()
    Fields.forDisplayedFields { case field: String =>
      out.append(Fields.layout(field)(field))
      out.append(" ")
    }
    out.toString
  }

  /** Length of the current header */
  def headerLength: Int = theHeader.value.length

  /** The (current) ordering of the rows in the viewer */
  var theOrdering: Seq[Row] => Seq[Row] = Orderings.byName

  /** The rows derived from `folder` to be shown in the viewer */
  def theRows: Seq[Row] = {
    val sort:    Seq[Row] => Seq[Row] = if (reverseSort) theOrdering.andThen(Orderings.reverseView(_)) else theOrdering
    val filter:  Seq[Row] => Seq[Row] = if (includeInvisible) {
      case rows: Seq[Row] => rows
    } else {
      case rows: Seq[Row] => rows.filterNot(_.isHidden)
    }
    val dirs  = sort(filter(folder.allDirectories))
    val files = sort(filter(folder.allFiles))
    // Potential for speedup by pushing dirs/files into folder: hardly worth the complexity
    dirs++files
  }

  /**
   * @return the current presentation of the rows to be displayed in `viewer`.
   */
  def theListing: CachedSeq[Row, String] = folder.withValidCaches {
    val rows = theRows
    setRowCount(rows.length)
    CachedSeq(rows){
      row: Row =>
        import files.FileAttributes._
        val out = new StringBuilder()
        Fields.forDisplayedFields { case field: String =>
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
            case "Parent"   => layout(relativePath(row.path.getParent))
            case "###"      => layout(row.path.getNameCount.toString)
            case _          => ""
          }
          out.append(text)
          out.append(" ")
        }
        out.toString
    }
  }

  def setRowCount(count: Int): Unit = Settings.countLabel.set(count.toString)

  def relativePath(parent: Path): String = {
    parent.getFileName.toString
  }

  /** The paths selected in the viewer */
  def selectedPaths: Seq[Path] = viewer.selectedRows.map { i => theRows(i).path }

  /** The glyph that shows the current presentation */
  lazy val viewer: unstyled.dynamic.SeqViewer = new unstyled.dynamic.SeqViewer(
    headerLength + 1, 36,
    fileSheet.buttonFont,
    fileSheet.buttonForegroundBrush,
    fileSheet.buttonBackgroundBrush,
    Brushes.red,
    theListing,
    autoScale = true,
    header = List(theHeader.value)) {

    override def isUnderlined(i: Int): Boolean = Shelf.contains(theRows(i).path)

    val cutBrush: Brush = fileSheet.buttonForegroundBrush.sliced(2, 3f)

    override def underlineBrush: Brush = if (Shelf.forCut) cutBrush else fileSheet.buttonForegroundBrush

    override def onDoubleClick(mods: Bitmap, selected: Int): Unit = {
      val path = folder.path.resolve(theRows(selected).path)
      if (logging) finest(s"onDoubleClick(#$selected=$path) on $thisViewer")
      services.openPath(path)
      clearSelection()
    }

    override def onHover(mods: Bitmap, hovered: Int): Unit = if (false) folder.withValidCaches {
      if (theRows.isDefinedAt(hovered)) {
        val path = theRows(hovered).path.toString
        if (logging) finest(s"onHover(#$hovered=$path)")
        if (logging) finest(path)
      } else {
        if (logging) finest(s"onHover(#$hovered=???)")
      }
    }

    override def onOther(gesture: Gesture): Unit = {
      import GlyphTypes.Key

      import gesture._
      gesture match {

        case Keystroke(Key.LEFT, _) =>
          if (selectedPaths.length==1) {
            val path = folder.path.resolve(selectedPaths.head)
            val parent = path.getParent
            if (parent ne null) (thisViewer.services.openPath)(parent)
          } else bell.play()

        case Keystroke(Key.C, _) if PRESSED =>
          thisViewer.shelf(forCut = false)

        case Keystroke(Key.X, _) if PRESSED =>
          thisViewer.shelf(forCut = true)

        case Keystroke(Key.BACKSPACE, _) if PRESSED =>
          if (CONTROL) thisViewer.delete() else thisViewer.trash()

        case Keystroke(Key.V, _) if PRESSED =>
          thisViewer.paste()

        case Keystroke(Key.EQUALS, _) if PRESSED && CONTROL =>
          val diagonal = thisViewer.GUI.guiRoot.diagonal
          thisViewer.GUI.guiRoot.setContentSize(diagonal+(20,20))

        case Keystroke(Key.MINUS, _) if PRESSED && CONTROL =>
          val diagonal = thisViewer.GUI.guiRoot.diagonal
          thisViewer.GUI.guiRoot.setContentSize(diagonal-(20,20))


        case Keystroke(Key.N, _) if PRESSED =>
          val thePath: Path = selectedPaths.length match {
            case 0 => folder.path
            case 1 => selectedPaths.head
            case _ => { bell.play(); folder.path }
          }
          thisViewer.services.openServices(thePath)

        case Keystroke(Key.D, _) if PRESSED =>
          val thePath: Path = selectedPaths.length match {
            case 0 => folder.path
            case 1 => selectedPaths.head
            case _ => { bell.play(); folder.path }
          }
          val path = thePath.getParent
          if (path ne null) thisViewer.services.openServices(path)


        //case Keystroke(Key.P, _) if PRESSED =>
        //  Actions.print()

        case other =>
          if (logging) warn(s"onOther($gesture) on $thisViewer")
          bell.play()
      }
    }
  }

  def close(): Unit = {
    folder.folderChanged.removeTagged(thisViewer)
    folder.patternAssigned.removeTagged(thisViewer)
    if (logging) finest(s"Closing folder for ${folder.path}")
    folder.close()
  }

  /**
   * Provides a `GUI` consisting of a sequence of buttons -- each corresponding to an ancestor of this
   * folder in the filestore, and labelled with its filename (not its path). Pressing a button
   * opens a viewer on the ancestor. The ".." button opens a viewer for the  real path to this folder.
   * The "/" button opens a viewer for the root folder.
   *
   */
  object PathButtons {
    val hintSheet:   StyleSheet = Barents.hintSheet
    val buttonSheet: StyleSheet = Barents.pathButtonSheet

    private lazy val prefixDirs = folder.prefixPaths.toSeq.scanLeft(Path.of("/"))((b, p) => b.resolve(p))

    private lazy val buttons: Seq[Glyph] = prefixDirs.tail.map {
      case path =>
        TextButton(s"${path.getFileName.toString}", hint = Hint(3, path.toRealPath().toString, preferredLocation = Centred)(hintSheet)) { _ => services.openPath(path) }(buttonSheet)
    }.prepended(TextButton(s"/") { _ => services.openPath(Path.of("/")) }(buttonSheet)).appended(fileSheet.hSpace()).appended(parent)

    private lazy val parent = {
      val up = folder.path.resolve("..").toRealPath()
      TextButton("..", hint = Hint(3, s"Open the real parent directory\n${up.toString}",
                                      preferredLocation = Centred)(hintSheet)) { _ => services.openPath(up) }(buttonSheet)
    }

    lazy val GUI: Glyph = NaturalSize.Row(align = Mid)(buttons)

  }

  /**
   * 
   * Provides a `GUI` composed of (suitably-captioned) checkboxes that control whether a field is to be
   * presented (or not) in the current folder listing.
   * 
   * 
   */
  object Fields {

    import org.sufrin.utility.CharSequenceOperations._

    type FieldLayout = CharSequence => CharSequence
    /** The list of potential fields (in order of presentation) */
    val allFields: Seq[String] =
      List("Id",
           "Name",
           "Parent",  "###",
           "Kind",
           "Size", "Size+", "Perms",
           "#",
           "Owner", "Group", "Created",
           "Modified", "Accessed")

    /** The list of potential layouts (in order of presentation) */
    val fieldLayouts: Seq[FieldLayout] =
      List(rightJustify(8)(_),
           leftJustify(FileAttributes.rowNameLength + 1)(_),
           leftJustify(20)(_),
           centerJustify(3)(_),
           leftJustify(12)(_),
           rightJustify(10)(_), rightJustify(10)(_), rightJustify(10)(_),
           centerJustify(2)(_),
           rightJustify(10)(_), rightJustify(10)(_), centerJustify(21)(_),
           centerJustify(21)(_), centerJustify(21)(_))
    

    /** The set of fields currently selected for displaying */
    val displayed: mutable.Set[String] = mutable.Set[String]("Name", "Size+", "Modified", "Owner", "Permissions", "Accessed")

    /** apply `action`  to each field selected for display */
    def forDisplayedFields(action: String => Unit): Unit = {
      for {disp <- allFields if displayed.contains(disp)} action(disp)
    }

    lazy val GUI: Glyph = {
      val labels: Seq[Glyph] = allFields.map(label => Label(label))
      val toggles: Seq[Glyph] = allFields.map { label =>
        CheckBox(displayed.contains(label)) {
          case false => displayed.remove(label); viewer.refresh(theListing)
          case true => displayed.add(label); viewer.refresh(theListing)
        }
      }
      Grid(width = labels.length, padx = 10).table(labels ++ toggles)
    }

    /** Maps a field name to its default layout */
    private val fieldLayout: Map[String, FieldLayout] = ListMap.from(allFields zip fieldLayouts)

    /*
     *  Effective mapping from `fieldName` to `FieldLayout`
     */
    def layout(fieldName: String): FieldLayout =
      fieldName match {
        case "Owner" => centerJustify(folder.ownerWidth max 5)(_)
        case "Group" => centerJustify(folder.groupWidth max 5)(_)
        case _ => fieldLayout(fieldName)
      }
  }

  /** Provides a button (`GUI`) that pops up the settings sheet for the current view */
  object Settings {
    import PathButtons.hintSheet
    private val invisibilityCheckBox = {
      styled.Label(" Invisible ") above
      styled.CheckBox(includeInvisible, hint = Hint(2, if (includeInvisible) "Click not to show invisibles" else "Click to show invisibles", constant=false, preferredLocation = Centred)) {
        state =>
          includeInvisible = state
          theListing.clear()
          viewer.refresh(theListing, reset = true)
      }
    }

    val pathDepthSymbol = "###"
    private val reverseSortCheckBox =
      styled.SymbolicCheckBox(reverseSort, whenTrue = "descending", whenFalse = "ascending", hint = Hint(2, "Reverse the\ncurrent sort\norder", preferredLocation = Centred)) {
        state =>
          reverseSort = state
          theListing.clear()
          viewer.refresh(theListing, reset = true)
      }.roundFramed(fg=Brushes.darkGrey(width=2), radius=15)

    private lazy val sortButtons: RadioCheckBoxes = RadioCheckBoxes(
      captions = List("Name", "Parent",  "###", "Size", "Created", "Modified", "Accessed"),
      prefer = "Name"
      ) { case selected => selected match {
      case Some(0) => theOrdering = Orderings.byName
      case Some(1) => theOrdering = Orderings.bySize
      case Some(2) => theOrdering = Orderings.byCreateTime
      case Some(3) => theOrdering = Orderings.byModTime
      case Some(4) => theOrdering = Orderings.byAccessTime
      case Some(5) => theOrdering = Orderings.byPathDepth
      case Some(6) => theOrdering = Orderings.byParentPath
      case _ => sortButtons.select(0); theOrdering = Orderings.byName
    }
      theListing.clear()
      viewer.refresh(theListing)
    }

    private lazy val orderButtons: Glyph =
      NaturalSize.Row(align = Mid)(
        List(Label("Ordered on "), reverseSortCheckBox, Label(" ")) ++ sortButtons.glyphButtons(Right, fixedWidth = false)
      )

    private lazy val settingsPanel: Glyph = {
      val close = Barents.settingsCloseIcon
      val closeSettings = unstyled.reactive.RawButton(close, close, close) { _ => settingsDialogue.close() }
      NaturalSize.Col(align = Left)(
        closeSettings,
        FixedSize.Row(width = GUI.guiRoot.w-10, align = Mid)(fileSheet.hFill(), Fields.GUI, invisibilityCheckBox, fileSheet.hFill()),
        fileSheet.ex,
        FixedSize.Row(width = GUI.guiRoot.w-10, align = Mid)(fileSheet.hFill(), orderButtons, fileSheet.hFill()), fileSheet.vSpace()
        )
    }

    private lazy val settingsDialogue: Dialogue[Unit] =
      overlaydialogues.Dialogue.POPUP(settingsPanel, Seq.empty, closeGlyph=None).AtTop(GUI.guiRoot)

    val countLabel: ActiveString = ActiveString("xxxx")

    val GUI: Glyph = {
      val closeIcon:    Glyph = Barents.viewCloseIcon
      val settings: Glyph = Barents.viewSettingsIcon
      NaturalSize.Col(align=Center)(
          MenuGlyphButton(settings, settings, settings, hint=Hint(0.75, "Change view settings", preferredLocation = West)(hintSheet)){
            _ =>
              settingsDialogue.canEscape = true
              settingsDialogue.isMenu = true
              settingsDialogue.start()
          }(fileSheet),
          MenuGlyphButton(closeIcon, closeIcon, closeIcon, hint=Hint(0.75, "Close this view &\nrevert to parent view", preferredLocation = West)(hintSheet)){
            _ =>
              Viewer.fine(s"Closing from ${folder.path}")
              services.closeExplorer(folder.path)
          }(fileSheet),
          countLabel.framed()
      )
    }

  }

  def giveupFocus(): Unit = {
    if (GUI.hasGuiRoot) GUI.guiRoot.giveupFocus()
  }

  object Pattern {

    def setPattern(pattern: String, tree: Boolean = false): Unit = {
      val effectivePattern = if (pattern.isBlank) "" else pattern // s"${folder.path.toString}/$pattern"
      folder.setPattern(effectivePattern, tree) match {
        case None =>
        case Some(error) =>
          styled.windowdialogues.Dialogue.OK(Label(s"$error").enlarged(10), title = "Pattern error").InFront(GUI).start()
      }
    }

    val patternField: TextField  = TextField(size = 20, onCursorLeave = { pat => setPattern(pat, searchTree) }, onEnter = { pat => setPattern(pat, searchTree) } )(PathButtons.buttonSheet)
    val patternSheet: StyleSheet = fileSheet.copy(buttonDecoration = styles.decoration.unDecorated)
    var searchTree: Boolean = false



    case class SavedSettings(displayed: Set[String] = Fields.displayed.toSet, ordering: Seq[Row] => Seq[Row] = theOrdering)

    var manualSettings = SavedSettings()

    val filterButtons: RadioCheckBoxes = RadioCheckBoxes(
      captions = List("Filter", "find", "Find", "FIND"),
      prefer = "Filter"
      ) {
      case Some(n) if n>0  =>
        val depth = n match { case 1=>4; case 2=>6; case 3=>Int.MaxValue }
        searchTree = true
        manualSettings = SavedSettings()
        Fields.displayed.add("Parent")
        Fields.displayed.add("###")
        theOrdering = Orderings.byPathDepth
        setPattern(patternField.string, searchTree)
        viewer.refresh(theListing)

      case Some(0) | _ =>
        searchTree = false
        Fields.displayed.clear()
        Fields.displayed.addAll(manualSettings.displayed)
        theOrdering = manualSettings.ordering
        setPattern(patternField.string, searchTree)
        viewer.refresh(theListing)
    }

    def streamMatches(patternSource: String): Unit = {
        import org.sufrin.microCSO._

        val paths: Chan[(Path, BasicFileAttributes)] = Chan[(Path, BasicFileAttributes)](1)

        val (depth, pat)  = {
          patternSource match {
            case s"$pat%$num" if num.matches("[0-9]+") => (num.toInt, pat)
            case _ => (Int.MaxValue, patternSource)
          }
        }

        val matcher = FileSystems.getDefault.getPathMatcher(s"glob:${folder.path.toString}/$pat")

        val readPaths = proc {
            val rootDepth = folder.path.getNameCount
            proc.repeatedly {
              paths ? {
                pair => if (pair == null) SourceDefault.error ("PAIR null") else {
                  val (path, attrs) = pair
                  val depth = path.getNameCount-rootDepth
                  if (matcher.matches(path)) println((s"${"  " * depth} ${path.getFileName}"))
                } }
            }
            paths.close()
        }


        println(s"Depth: $depth, pattern: $pat")
        try {
          val handle = (files.Directory.streamAll(folder.path, depth, followSymbolic=true, includeRoot=false, paths) || readPaths).fork()
          println(handle)
        }
        catch {
          case exn: Throwable => exn.printStackTrace()
        }
    }


      val GUI: Glyph =
        NaturalSize.Row(align = Mid) (
          patternField.enlarged(10).roundFramed(radius = 10).enlarged(10f),
          SimpleGlyphButton(IconLibrary.XSHAPE(patternSheet.exHeight, Brushes("darkgrey.3)")), hint = Hint(1, "Clear pattern")) {
            _ =>
              Fields.displayed.clear()
              Fields.displayed.addAll(manualSettings.displayed)
              theOrdering = manualSettings.ordering
              setPattern("", false)
          }(patternSheet),

          SimpleGlyphButton(IconLibrary.LOOKSHAPE(patternSheet.exHeight, Brushes("darkGrey.2.stroke)")).turned(45),
                            hint = Hint(1, if (searchTree) "find matches" else "filter matches", constant = false)) {
            _ =>
              setPattern(patternField.string, searchTree)
          }(patternSheet),
          NaturalSize.Row(align = Mid)(filterButtons.glyphButtons(Left, fixedWidth = false)),

          //TextButton("Stream"){
          //  _ => streamMatches(patternField.string)
          //}(patternSheet)

        )

        locally {
      // solicit pattern-change notifications from the folder
      // (this synchronises the patterns of all viewers of a given folder)
      folder.patternAssigned.handleWithTagged(thisViewer) {
        case patternSource => patternField.string = patternSource
      }
    }
  }


  def setTitle(path: Path): Unit = {
    import PathProperties._
    if (GUI.hasGuiRoot) GUI.guiRoot.rootWindow.setTitle(path.abbreviatedString())
  }

  lazy val GUI: Glyph = NaturalSize.Col(align=Center)(
    { val hfill = fileSheet.hFill()
      val pathWidth = viewer.w - Settings.GUI.w - hfill.w
      val scale = if (PathButtons.GUI.w<pathWidth) 1 else pathWidth/PathButtons.GUI.w
      FixedSize.Row(width=viewer.w, align=Mid)(PathButtons.GUI.scaled(scale), hfill, Settings.GUI).enlarged(15).roundFramed(radius=20)
    },
    Pattern.GUI,
    viewer
  )
}


object Viewer extends SourceLoggable {

  var dialogueAnchor: Glyph = INVISIBLE()

  private var _serial = 0
  def nextSerial(): Int = { _serial +=1 ; _serial }

  import GlyphTypes.FontStyle.NORMAL

  implicit lazy val fileSheet: StyleSheet = Barents.fileSheet

  val settingsIcon: Glyph = External.Image.readResource("/settings").scaled(0.03f)

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
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to open\n${path.toString}"))(dialogueSheet).InFront(dialogueAnchor).start(floating = false)
        }
      else
        styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop open not supported")).InFront(dialogueAnchor).start()
    }
    else
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop not supported on this platform")).InFront(dialogueAnchor).start()
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
            styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Failed to browse (for printing)\n${path.toRealPath()}"))(dialogueSheet).InFront(dialogueAnchor).start(floating = false)
        }
      else
        styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop print not supported")).InFront(dialogueAnchor).start()
    }
    else
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Desktop not supported on this platform")).InFront(dialogueAnchor).start()
    } else {
      styled.windowdialogues.Dialogue.OK(dialogueLabel(s"Unreadable, or folder\n$path"))(dialogueSheet).InFront(dialogueAnchor).start()
    }
  }

}





