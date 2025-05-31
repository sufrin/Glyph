package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{BlendMode, PaintMode, PaintStrokeCap, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import io.github.humbleui.types.Rect
import org.sufrin.{glyph, logging}
import org.sufrin.glyph.Brushes.{black, blue, green, lightGrey, red, white, yellow}
import org.sufrin.glyph.GlyphShape.{arrow, asGlyph, circle, composite, lineBetween, polygon, rect, FILL, PathShape, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet, GlyphButton}
import org.sufrin.glyph.styles.decoration.{unDecorated, Framed}
import org.sufrin.glyph.unstyled.reactive.{Enterable, Reaction}
import org.sufrin.glyph.unstyled.static.FilledPolygon

import java.io.File
import scala.collection.mutable


object PathSymbols {
  val blend: BlendMode = BlendMode.SRC
  val exx = new PathShape(red(width=0, blendMode=blend)) {
    moveTo(0, 0)
    lineTo(20, 20)
    moveTo(0, 20)
    lineTo(20, 0)
  }
  val pie = new PathShape(red(width=0, blendMode=blend)) {
    moveTo(0, 0)
    lineTo(10, 10)
    lineTo(0, 20)
    moveTo(10, 10)
    lineTo(20, 10)
  }
}

/**
 * Drawing board
 */
class DrawingBoard(background: Glyph)(left: Variable[Int], right: Variable[Int]) extends  GestureBasedReactiveGlyph { thisBoard =>

  var lastMouse, lastMouseDown: Vec = background.diagonal * 0.5f

  val displayList: mutable.Queue[TargetShape] = new mutable.Queue[TargetShape]()

  var selection: Seq[TargetShape] = Seq.empty

  var vertices: Seq[Vec] = Seq.empty

  var lastPathOrigin: Vec = Vec.Origin

  var deletion: Seq[TargetShape] = Seq.empty

  val bell = Sound.Clip("WAV/glass.wav")


  object UndoRedo {

    case class State(
                      culprit: String, // the command that caused this state to be saved
                      lastPathOrigin: Vec,
                      lastMouse: Vec,
                      lastMouseDown: Vec,
                      vertices: Seq[Vec],
                      displayList: Seq[TargetShape],
                      selection: Seq[TargetShape]
                    )


    def copyState(culprit: String): State = State(culprit, lastPathOrigin, lastMouse, lastMouseDown, vertices, displayList.toSeq.map(_.copyState), selection.map(_.copyState))

    def restoreState(state: State): Unit = {
      thisBoard.lastPathOrigin = state.lastPathOrigin
      thisBoard.lastMouse = state.lastMouse
      thisBoard.lastMouseDown = state.lastMouseDown
      thisBoard.vertices = state.vertices
      thisBoard.displayList.clear();
      thisBoard.displayList.enqueueAll(state.displayList)
      thisBoard.selection = state.selection
    }

    var states, unstates: mutable.Stack[State] = new mutable.Stack[State]

    /** Push the current state onto the undo stack unless the command is mergeable (starts with a .) and matches the topmost culprit. */
    def withState(command: String)(action: => Unit): Unit = {
      if (command.startsWith(".") && states.nonEmpty && states.top.culprit == command) {
        //logging.Default.warn(s"$command merged.")
      } else {
        states.push(copyState(command))
      }
      unstates.clear()
      action
    }

    def redo() = {
      if (unstates.nonEmpty) {
        val state = unstates.pop()
        restoreState(state)
        states.push(state)
      } else bell.play()
    }

    def undo() = {
      if (states.nonEmpty) {
        val prevState = states.pop()
        unstates.push(copyState(prevState.culprit))
        restoreState(prevState)
      } else bell.play()
    }

    def feedback(): Unit = {
      left.set(states.length)
      right.set(unstates.length)
      reDraw()
    }

    def undoHint: String = states.toSeq.take(4).map(_.culprit).mkString(" ")
    def redoHint: String = unstates.toSeq.take(4).map(_.culprit).mkString(" ")

    def clear(): Unit = {
      states.clear()
      unstates.clear()
    }
  } // UndoRedo

  import UndoRedo.{undo, redo, withState}

  def undoHint() = UndoRedo.undoHint
  def redoHint() = UndoRedo.redoHint

  /** For external invocation of an action that requires feedback and redrawing */
  def fromButton(command: String = "")(action: => Unit): Unit = {
    if (command != "unrecorded") withState(command) {
      action
    } else action
    feedback()
  }

  def selected(location: Vec): Seq[TargetShape] = displayList.toSeq.filter(_.isBeneath(location))

  def hovered(location: Vec): Seq[TargetShape] = displayList.toSeq.filter(_.isBeneath(location))

  def feedback(): Unit = {
    UndoRedo.feedback()
    reDraw()
  }

  def restart(): Unit = {
    UndoRedo.clear()
    displayList.clear()
    selection = Seq.empty
    vertices=Seq.empty
    feedback()
  }

  def clean(): Unit = {
    println(displayList)
    UndoRedo.clear()
    selection = Seq.empty
    vertices=Seq.empty
    feedback()
    println(displayList)
  }



  /** Apply the transform, as if about the current centre  */
  def transformSelected(transform: GlyphShape => GlyphShape, command: String=""): Unit = withState (command) {
    deletion = selection
    val mapped = selection.map {
      case v: TargetShape =>
        val v$ = transform(v.shape)
        val delta = v.diagonal - v$.diagonal
        v$.targetLocatedAt(v.x+delta.x/2, v.y+delta.y/2)
    }
    displayList.dequeueAll(_.isIn(selection))
    deletion = selection
    displayList.enqueueAll(mapped)
    selection = mapped
  }

  class Composition(x : Scalar, y : Scalar, shape : GlyphShape, val components: Seq[TargetShape]) extends TargetShape(x, y, shape)

  /** compose the selected elements in the order of selection  */
  def composeSelected(compose: GlyphShape => GlyphShape => GlyphShape): Unit =
    if (selection.length>1) withState ("compose") {
    var composed: GlyphShape = selection.head.shape
        for { v <- selection.tail } composed = compose(composed)(v.shape)
        val (x, y) = (selection.head.x, selection.head.y)
        displayList.dequeueAll(_.isIn(selection))
        val composite = new Composition(x, y, composed, selection)
        displayList.enqueue(composite)
        selection = List(composite)
      }
    else bell.play()

  def InPlace(targets: Seq[TargetShape]): GlyphShape = new GlyphShape {
    val left  = targets.map(_.x).min
    val right = targets.map{ t => t.x+t.w }.max
    val bot = targets.map{ t => t.y+t.h }.max
    val top = targets.map(_.y).min
    val placed = targets.map{ t => t.shape.locatedAt(t.x-left, t.y-top)}

    def draw(surface: Surface): Unit = {
        for { glyph <- placed } glyph.draw(surface)
    }

    def diagonal: Vec = Vec(right-left, bot-top)

    def withForeground(brush: Brush): GlyphShape = null
  }

  def composeSelectedInPlace(): Unit =
    if (selection.length>1) withState ("compose") {
      val (x, y) = (selection.head.x, selection.head.y)
      val composite = new Composition(x, y, InPlace(selection), selection)
      displayList.dequeueAll(_.isIn(selection))
      displayList.enqueue(composite)
      selection = List(composite)
    }
    else bell.play()

  def decomposeSelected(): Unit =
    if (selection.length == 1) {
      selection.head match {
        case composition: Composition =>
          withState ("decompose") {
            displayList.dequeueAll(_.equals(selection.head))
            selection = composition.components
            displayList.enqueueAll(selection)
          }
        case _ => bell.play()
      }
    } else bell.play()

  val UndoGesture: Gesture = Keystroke(Key.Z, Pressed|Control)
  val RedoGesture: Gesture = Keystroke(Key.Z, Shift|Pressed|Control)

  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    //println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val PRESSED       = mods.includeAll(Pressed)
    val CONTROL       = mods.includeSome(Command|Control)
    val PRIMARY       = mods.includeAll(Primary   | Pressed)
    val SECONDARY     = mods.includeAll(Secondary | Pressed) || (PRIMARY && CONTROL)
    val COMPLEMENT    = mods.includeSome(Shift)
    val SHIFT         = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(thisBoard)
      case _: MouseLeaves => guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if CONTROL => transformSelected(_.scale(if (delta.x+delta.y>0) 1/1.05f else 1.05f), ".scale")
      case _: MouseScroll            => transformSelected(_.turn((delta.x+delta.y).sign*5, COMPLEMENT), ".turn")

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {
          case Key.LEFT  =>  transformSelected(_.turn(-90, COMPLEMENT), ".turn")
          case Key.RIGHT =>  transformSelected(_.turn(90, COMPLEMENT), ".turn")

          case Key.MULTIPLY  =>  transformSelected(_.scale(1.05f), ".scale")
          case Key.SLASH     =>  transformSelected(_.scale(1/1.05f), ".scale")

          case Key.PERIOD =>  transformSelected(_.turn(5, COMPLEMENT), ".turn")

          case Key.Z if CONTROL && SHIFT=> redo()

          case Key.Z if CONTROL => undo()

          case Key.A if CONTROL =>
            withState (if (COMPLEMENT) "complement" else "select") {
              if (COMPLEMENT)
                selection = displayList.toSeq.filter(_.notIn(selection))
              else
                selection = displayList.toSeq
            }

          case Key.HOME  => withState ("deselect") { selection = Seq.empty }

          case Key.X | Key.V =>
            withState ("vertex") { addVertex(lastMouseDown) }

          case Key.DELETE | Key.BACKSPACE =>
            withState (if (COMPLEMENT) "delete complement" else "delete") {
              if (COMPLEMENT) {
                deletion = displayList.toSeq.filter(_.notIn(selection))
                displayList.dequeueAll(_.notIn(selection))
              } else {
                displayList.dequeueAll(_.isIn(selection))
                deletion = selection
              }
              selection = Seq.empty
            }


          // ignore shift buttons
          case Key.SHIFT | Key.CONTROL | Key.CAPS_LOCK | Key.ALT | Key.MAC_COMMAND | Key.MAC_FN | Key.MAC_OPTION =>

          case _ =>
            bell.play()
        }

      case MouseClick(_)  if (PRESSED && COMPLEMENT) => withState ("vertex") {
        lastMouseDown = location
        addVertex(location)
      }

      case MouseMove(_) =>
        if (PRESSED) {
          withState (".move") { for { shape <- selection } shape.moveBy(delta.x, delta.y) }
        }
        lastMouse = location

      case MouseClick(_) if (PRIMARY) => withState ("click-p") {
        selection = selected(location)
        lastMouseDown = location
        lastMouse = Vec.Origin
      }

      case MouseClick(_) if (SECONDARY) => withState ("click-s") {
          val touched = selected(location)
          for {shape <- touched}
            if (selection contains shape)
              selection = selection.filterNot(_.eq(shape))
            else
              selection = selection ++ List(shape)

        lastMouseDown = location
        lastMouse = Vec.Origin
      }

      case MouseClick(_) =>

    }
    //println(displayList)
    //println(selection)
    feedback()
    reDraw()
  }

  def copy(fg: Brush=fg, bg: Brush=bg): DrawingBoard = new DrawingBoard(background)(left, right)

  val fg: Brush = background.fg
  val bg: Brush = background.bg

  def diagonal: Vec = background.diagonal

  val focussedBrush: Brush      = Brushes.red(width=4, mode=GlyphShape.STROKE)
  val focussedFrame: GlyphShape = GlyphShape.rect(background.w-4, background.h-4)(focussedBrush)

  val selectBrush: Brush = Brushes("white/2/ROUND")(mode=PaintMode.STROKE)


  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    surface.declareCurrentTransform(thisBoard)
    drawBackground(surface)
    background.draw(surface)

    for { shape <- displayList } {
      shape.setHovering(shape.isBeneath(lastMouse)||shape.canHandle(lastMouse))
      shape.setSelected(selection contains shape)
      shape.draw(surface)
    }

    indicateSelection(surface)
    indicateVertices(surface)

    if (guiRoot.hasKeyboardFocus(thisBoard)) focussedFrame.draw(surface)
  }

  val selectionPath = new GlyphShape.PathShape(selectBrush, false)
  val vertexPath = new GlyphShape.PathShape(Brushes("red/2.stroke-4-4")(blendMode = BlendMode.XOR), false)


  def indicateVertices(surface: Surface): Unit = if (vertices.nonEmpty) {
    vertexPath.reset()
    for { Vec(x,y) <- vertices.take(1) } vertexPath.moveTo(x,y)
    for { Vec(x,y) <- vertices.drop(1) } vertexPath.lineTo(x,y)
    for { Vec(x,y) <- vertices.take(1) } {
      vertexPath.lineTo(x,y)
      //vertexPath.addCircle(x, y, 10)
      vertexPath.addPathShape(PathSymbols.exx, x, y)
    }
    // mark the centre
    val b = (vertexPath.path.computeTightBounds())
    val w = b.getWidth
    val h = b.getHeight
    vertexPath.addPathShape(PathSymbols.pie, b.getLeft+w/2, b.getTop+h/2)
    // draw the path
    surface.withOrigin(b.getLeft, b.getTop) { vertexPath.draw(surface) }
    lastPathOrigin=Vec(b.getLeft, b.getTop)
  }


  def indicateSelection(surface: Surface): Unit = {
    selectionPath.reset()
    selection.length match
    { case 0 =>
      case 1 =>
        for { shape <- selection.take(1) } {
          selectionPath.addCircle(shape.x+shape.w/2, shape.y+shape.h/2, 10)
        }
      case _ =>
        for { shape <- selection.take(1) } selectionPath.moveTo(shape.x+shape.w/2, shape.y+shape.h/2)
        for { shape <- selection.drop(1) } selectionPath.lineTo(shape.x+shape.w/2, shape.y+shape.h/2)
        for { shape <- selection.take(1) } {
          selectionPath.addCircle(shape.x+shape.w/2, shape.y+shape.h/2, 10)
        }
    }
    val b = (selectionPath.path.computeTightBounds())
    surface.withOrigin(b.getLeft, b.getTop) { selectionPath.draw(surface) }
    surface.withOrigin(lastMouseDown-(1,1)) { circle(2)(blue).draw(surface)}
  }

  def addShape(kind: String, x:Scalar, y:Scalar)(shape: GlyphShape) : Unit = withState(s"+$kind"){
    val target = shape.targetLocatedAt(x min (w - shape.w), y min (h - shape.h))
    displayList.enqueue(target)
    selection = List(target)
  }

  def addShape(kind: String, shape: GlyphShape) : Unit = {
    val delta = shape.diagonal * 0.5f
    addShape(kind, lastMouseDown.x-delta.x, lastMouseDown.y-delta.y)(shape)
  }

  def addVertex(location: Vec): Unit = {
    vertices = vertices++List(location)
  }

  def addPoly(fg: Brush) : Unit = if (vertices.nonEmpty) {
    addShape("poly", lastPathOrigin.x,lastPathOrigin.y)(polygon(vertices)(fg))
  }
}



object Play extends Application {
  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()
  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=styles.decoration.Framed(fg=blue(width=2), enlarge=0.7f, radius=0.5f),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = Brushes.white
  )
  def title = s"""Play"""
  override
  val defaultIconPath: Option[String] = Some ("PNG/WorcesterCrest.png")

  val book = Book()
  val Page = book.Page
  implicit val sheet: StyleSheet= interfaceStyle
  implicit val bookSheet: BookSheet = BookSheet(sheet, sheet)

  val Interact = Page("Interact", ""){
    import sheet.{ex,em}
    val left, right = styled.ActiveString("   ")
    val done: Variable[Int]   = Variable(0){ case n => left.set(f"$n%03d") }
    val undone: Variable[Int] = Variable(0){ case n => right.set(f"$n%03d") }


    val drawingBoard = new DrawingBoard(rect(1200, 800)(lightGrey))(done, undone)

    var newBrush: Brush = Brushes("blue.stroke.round.width(4)")

    def but(kind: String)(shape: => GlyphShape): Glyph = {
      val b  = styled.TextButton(kind){ _ => { drawingBoard.addShape(kind, shape); drawingBoard.feedback() } }
        HintManager(b.asInstanceOf[Enterable], 5, ()=>newBrush.toString, false)
        b
    }

    val blackArrow =  arrow(black)

    var width, height, radius: Scalar = 150
    var scale: Scalar = 1.0f
    var vertices: Int = 3

    val shapes: List[Glyph] = List(
      but("Rect")(rect(width,height)(newBrush.copy).scale(scale max 1)),
      but("Circ")(circle(radius)(newBrush.copy).scale(scale max 1)),
      but("Arrow")(arrow(newBrush.copy).scale(scale max 0.3f)),
      but("Star")(GlyphShape.polygon(PolygonLibrary.regularStarPath(vertices))(fg=newBrush.copy).scale(scale max 0.3f)),
      but("Poly")(GlyphShape.polygon(PolygonLibrary.regularPolygonPath(vertices))(fg=newBrush.copy).scale(scale max 0.3f)),

    )


    lazy val dimField: TextField = styled.TextField(onEnter = setDims, onCursorLeave = setDims, size = 50, initialText=s"w=$width, h=$height, r=$radius, scale=$scale, v=$vertices")


    def setDims(spec: String): Unit = {
      @inline def isFloat(s: String):Boolean = s.matches("[0-9]+([.][0-9]+)?")
      val specs = spec.toLowerCase.split("[ ]+|,[ ]*").toSeq
      for  { spec <- specs }
           spec match {
             case s"$v=$d" if v.nonEmpty && isFloat(d) =>
               val dim = d.toFloat
               v.head match {
                 case 'w' => width = dim
                 case 'h' => height = dim
                 case 'r' => radius = dim
                 case 's' => scale = dim
                 case 'v' => vertices = dim.toInt
                 case _ =>
                   logging.Default.warn(s"$spec does not specify w=, h=, r=, s=, v=")
               }

             case _ =>
               logging.Default.warn(s"$spec does not specify dimensions properly")
           }

      dimField.text=s"w=$width, h=$height, r=$radius, scale=$scale, v=$vertices"
    }

    def setNewBrush(spec: String): Unit = {
      if (spec.nonEmpty) newBrush = Brush.ofString(spec)
    }

    def showNewBrush(): Unit = {
      brushField.text = newBrush.toString
    }

    lazy val brushField = styled.TextField(size=40, onEnter=setNewBrush(_), initialText=newBrush.toString)

    val brushButton = styled.TextButton("Brush:"){ _ => setNewBrush(brushField.text) }(sheet.copy(buttonDecoration = Framed(fg=black(width=3), radius=50)))
    locally {
      HintManager(brushButton.asInstanceOf[Enterable], 5, ()=>newBrush.toString, false)
    }

    def undoHinted(targetButton: Glyph): Glyph = {
      HintManager(targetButton.asInstanceOf[Enterable], 5, drawingBoard.undoHint,  false)
      targetButton
    }

    def redoHinted(targetButton: Glyph): Glyph = {
      HintManager(targetButton.asInstanceOf[Enterable], 5, drawingBoard.redoHint, false)
      targetButton
    }

    def HintedButton(label: String, hint: String="")(action: Reaction): Glyph = {
      val button = styled.TextButton(label)(action)
      if (hint.nonEmpty)  HintManager(button.asInstanceOf[Enterable], 5, ()=>hint)
      button
    }

    NaturalSize.Col(align=Center)(
      drawingBoard,
      ex,
      FixedSize.Row(width=drawingBoard.w, align=Mid)(
        HintedButton("Restart", "Clear the board and start again"){ _ => drawingBoard.restart() },
        HintedButton("Clean", "Preserve the drawing and start again"){ _ => println(drawingBoard.displayList); drawingBoard.clean() },
        sheet.hFill(),

        HintedButton("o", "Compose selection in place")   { _ => {drawingBoard.composeSelectedInPlace()}},
        HintedButton("|", "Compose selection horizontally in selection order")   { _ => {drawingBoard.composeSelected(_.|||)}},
        HintedButton("-", "Compose selection vertically in selection order")   { _ => {drawingBoard.composeSelected(_.---)}},
        HintedButton("~", "Compose selection by superimposing in selection order")  { _ => {drawingBoard.composeSelected(_.~~~)}},
        HintedButton("↯", "Decompose the selected compsition" )   { _ => {drawingBoard.decomposeSelected()}},
        sheet.hFill(),
        HintedButton("Clear", "Clear the path design") { _ => drawingBoard.fromButton("-vertices") { drawingBoard.vertices = Seq.empty } },
        HintedButton("Path", "Make a path from the path design"){ _ => drawingBoard.fromButton("path") { drawingBoard.addPoly(newBrush.copy); drawingBoard.vertices = Seq.empty }},
        sheet.hFill(),
        HintedButton("Repaint", "Repaint the selected objects with the current brush"){ _ => drawingBoard.fromButton("recolour") { drawingBoard.transformSelected{ shape => shape.withForeground(newBrush.copy)}}},
        sheet.hFill(1, 2f),
        undoHinted(HintedButton("<"){ _ =>  drawingBoard.handle(drawingBoard.UndoGesture, Vec.Origin, Vec.Zero) }),
        left.framed(),
        styled.Label(" "),
        right.framed(),
        redoHinted(HintedButton(">"){_ => drawingBoard.handle(drawingBoard.RedoGesture, Vec.Origin, Vec.Zero) }),
        HintedButton("Help"){ _ =>}
      ),
      ex,
      FixedSize.Row(width=drawingBoard.w, align=Mid)(shapes),
      ex,
      brushButton beside brushField.framed() beside styled.Label(" ")  beside dimField.framed()
    ).enlarged(20)
  }

  val GUI: Glyph =  {
    import glyphXML.Language.translation._
    import sheet.{em,ex}
    NaturalSize.Col(align = Center)(
      <div width="70em" align="justify">
        <p align="center">GlyphShape Playground.</p>
      </div>,
      ex,
      book.Layout.leftCheckBoxes(Justify),
      ex
    )
  }

}
