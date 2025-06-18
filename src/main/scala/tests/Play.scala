package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{BlendMode, PaintMode, PaintStrokeCap, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import io.github.humbleui.types.Rect
import org.sufrin.{glyph, logging}
import org.sufrin.glyph.Brushes.{black, blue, green, lightGrey, red, white, yellow, NonBrush}
import org.sufrin.glyph.GlyphShape.{arrow, asGlyph, circle, composite, lineBetween, polygon, rect, FILL, PathShape, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet, GlyphButton, MenuButton, RadioCheckBoxes}
import org.sufrin.glyph.styles.decoration.{unDecorated, Framed}
import org.sufrin.glyph.unstyled.reactive.{Enterable, Reaction}
import org.sufrin.glyph.unstyled.static.{FilledPolygon, INVISIBLE}
import org.sufrin.glyph.Brush.{BUTT, ROUND, SQUARE}
import org.sufrin.glyph.Colour.HSV
import org.sufrin.glyph.NaturalSize.{transparent, Col, Grid, Row}
import org.sufrin.glyph.NaturalSize.Grid.Table
import org.sufrin.glyph.styled.windowdialogues.Dialogue.OK
import org.sufrin.glyph.unstyled.Text

import java.io.File
import java.lang.IllegalArgumentException
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

  val lastpie = new PathShape(blue(width=0, mode=STROKE, blendMode=blend)) {
    moveTo(0, 0)
    lineTo(10, 10)
    lineTo(0, 20)
    moveTo(10, 10)
    lineTo(20, 10)
  }

  val last = new PathShape(red(width=2, mode=STROKE)) {
    moveTo(0, 0)
    lineTo(10, 10)
    lineTo(0, 20)
    moveTo(20,20)
    lineTo(10, 10)
    lineTo(20, 0)
  }
}

/**
 * Drawing board
 */
class DrawingBoard(w: Scalar, h: Scalar, override val fg: Brush=transparent, override val bg: Brush=lightGrey)(left: Variable[Int], right: Variable[Int])(implicit style: StyleSheet) extends  GestureBasedReactiveGlyph { thisBoard =>

  var lastMouse, lastMouseDown: Vec = Vec(w, h) * 0.5f

  val displayList: mutable.Queue[TargetShape] = new mutable.Queue[TargetShape]()

  var selection: Seq[TargetShape] = Seq.empty

  var vertices: Seq[Vec] = Seq.empty

  var lastPathOrigin: Vec = Vec.Origin

  var cut: Seq[TargetShape] = Seq.empty

  val bell = Sound.Clip("WAV/glass.wav")


  object UndoRedo {

    case class State(
                      culprit: String, // the command that caused this state to be saved
                      lastPathOrigin: Vec,
                      lastMouse: Vec,
                      lastMouseDown: Vec,
                      vertices: Seq[Vec],
                      displayList: Seq[TargetShape],
                      selection: Seq[TargetShape],
                      cut: Seq[TargetShape]
                    )


    def copyState(culprit: String): State = State(culprit, lastPathOrigin, lastMouse, lastMouseDown, vertices, displayList.toSeq.map(_.copyState), selection.map(_.copyState), cut)

    def restoreState(state: State): Unit = {
      thisBoard.lastPathOrigin = state.lastPathOrigin
      thisBoard.lastMouse = state.lastMouse
      thisBoard.lastMouseDown = state.lastMouseDown
      thisBoard.vertices = state.vertices
      thisBoard.displayList.clear();
      thisBoard.displayList.enqueueAll(state.displayList)
      thisBoard.selection = state.selection
      thisBoard.cut = state.cut
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

    def undoHint: String = s"Undo ${states.toSeq.take(4).map(_.culprit).mkString(" ")} ${if (states.length>4) "..." else ""}"
    def redoHint: String = s"Redo ${unstates.toSeq.take(4).map(_.culprit).mkString(" ")} ${if (unstates.length>4) "..." else ""}"

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

  def selected(location: Vec): Seq[TargetShape] = displayList.toSeq.filter{target => target.isBeneath(location)||target.canHandle(location)}

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
    //cut = selection
    val mapped = selection.map {
      case v: TargetShape =>
        val v$ = transform(v.shape)
        val delta = v.diagonal - v$.diagonal
        v$.targetLocatedAt(v.x+delta.x/2, v.y+delta.y/2)
    }
    displayList.dequeueAll(_.isIn(selection))
    //cut = selection
    displayList.enqueueAll(mapped)
    selection = mapped
  }

  /**
   *  This `shape` was composed from the shapes of the given target components.
   *  @see composeSelected
   *  @see
   */
  class Composition(x : Scalar, y : Scalar, shape : GlyphShape, val components: Seq[TargetShape]) extends GlyphShape {
    override def toString: String = s"Composition[${super.toString}]"

    def draw(surface: Surface): Unit = shape.draw(surface)

    def diagonal: Vec = shape.diagonal

    override def scale(factor: Scalar): GlyphShape = new Composition(x, y, shape.scale(factor), components)

    override def turn(degrees: Scalar, tight: Boolean): GlyphShape = new Composition(x, y, shape.turn(degrees), components)
  }

  /** Compose the selected elements in the order of selection  */
  def composeSelected(compose: GlyphShape => GlyphShape => GlyphShape): Unit =
    if (selection.length>1) withState ("compose") {
      val targets = selection
      val left = targets.map(_.x).min
      val top = targets.map(_.y).min
      var composed: GlyphShape = targets.head.shape
        for { v <- targets.tail } composed = compose(composed)(v.shape)
        displayList.dequeueAll(_.isIn(selection))
        val composite = new Composition(left, top, composed, targets).targetLocatedAt(left, top)
        displayList.enqueue(composite)
        selection = List(composite)
      }
    else bell.play()

  /**
   * This shape's diagonal exactly spans the `targets'. Each
   * target shape is shown at the appropriate position
   * relative to the shape's origin.
   * @param targets
   */
  class ComposedInPlace(val targets: Seq[TargetShape]) extends GlyphShape {
    override def toString: String = s"InPlace(${targets.mkString(",\n")})"
    private val left  = targets.map(_.x).min
    private val right = targets.map{ t => t.x+t.w }.max
    private val bot = targets.map{ t => t.y+t.h }.max
    private val top = targets.map(_.y).min
    private val placed = targets.map{ t => t.shape.locatedAt(t.x-left, t.y-top)}

    def draw(surface: Surface): Unit = {
        for { glyph <- placed } glyph.draw(surface)
    }

    def diagonal: Vec = Vec(right-left, bot-top)

    def withForeground(brush: Brush): GlyphShape = new ComposedInPlace(targets.map(_.withForeground(brush)))

    def topLeft: Vec = Vec(left, top)
  }

  /**
   * Revise the Z order of the components: this
   * may make something visible that was occluded.
   */
  def zedOrder(): Unit = {
      @inline def area(t: TargetShape): Scalar = t.w*t.h
      def earlier(t1: TargetShape, t2: TargetShape): Boolean = area(t1)>=area(t2)
      withState("zed") {
        val targets = displayList.toSeq.sortWith(earlier)
        displayList.clear()
        displayList.enqueueAll(targets)
      }
  }

  /**
   * Compose the selection, and place the result at the top left corner of the selection's bounding box.
   */
  def composeSelectedInPlace(): Unit =
    if (selection.length>1) withState ("compose") {
      val inPlace = new ComposedInPlace(selection)
      val Vec(x, y) = inPlace.topLeft
      val composite = new Composition(x, y, inPlace, selection).targetLocatedAt(x, y)
      displayList.dequeueAll(_.isIn(selection))
      displayList.enqueue(composite)
      selection = List(composite)
    }
    else bell.play()

  def decomposeSelected(): Unit =
    if (selection.length == 1) {
      selection.head.shape match {
        case composition: Composition =>
          withState ("decompose") {
            displayList.dequeueAll(_.equals(selection.head))
            selection = composition.components
            displayList.enqueueAll(selection)
          }
        case other =>
          bell.play()
          logging.Default.error(s"cannot decompose $other")
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

      case _: MouseScroll if !CONTROL => transformSelected(_.scale(if (delta.x+delta.y>0) 1/1.05f else 1.05f), ".scale")
      case _: MouseScroll if CONTROL  => transformSelected(_.turn((delta.x+delta.y).sign*5, COMPLEMENT), ".turn")

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {
          case Key.LEFT  =>  transformSelected(_.turn(-90, COMPLEMENT), ".turn")
          case Key.RIGHT =>  transformSelected(_.turn(90, COMPLEMENT), ".turn")

          case Key.DIGIT8 if SHIFT  =>  transformSelected(_.scale(1.05f), ".scale")
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

          case Key.X  =>
            withState ("vertex") { addVertex(lastMouseDown) }

          case Key.C if selection.length==1 =>
            withState ("copy") { cut = selection }

          case Key.V if cut.length==1 =>
            withState ("paste") {
              val shape = cut.head.shape
              addShape("paste", shape)
              selection = cut
            }

          case Key.DELETE | Key.BACKSPACE =>
            withState (if (COMPLEMENT) "delete complement" else "delete") {
              if (COMPLEMENT) {
                cut = displayList.toSeq.filter(_.notIn(selection))
                displayList.dequeueAll(_.notIn(selection))
              } else {
                displayList.dequeueAll(_.isIn(selection))
                cut = selection
              }
              selection = Seq.empty
            }

          case Key.F1 =>
            withState (if (COMPLEMENT) "rehandle" else "unhandle") {
              if (COMPLEMENT) {
                for { shape <- selection } shape.handles.enabled = true
              } else {
                for { shape <- selection } shape.handles.enabled = false
              }
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
        if (PRESSED && selection.nonEmpty) {
          withState (".move") { for { shape <- selection } shape.moveBy(delta.x, delta.y) }
        }
        lastMouse = location

      case MouseClick(_) if (PRIMARY && !CONTROL) => withState (".primary") {
        selection = selected(location)
        if (selection.isEmpty)
          lastMouseDown = location
        else
          lastMouseDown = selection.head.centre
        lastMouse = Vec.Origin
      }

      case MouseClick(_) if (SECONDARY || (PRIMARY&&CONTROL)) => withState (".secondary") {
          val touched = selected(location)
          for {shape <- touched}
            if (selection contains shape)
              selection = selection.filterNot(_.eq(shape))
            else
              selection = selection ++ List(shape)

        lastMouseDown = if (selection.isEmpty) location else selection.head.centre
        lastMouse = Vec.Origin
      }

      case MouseClick(_) =>

    }
    //println(displayList)
    //println(selection)
    feedback()
    reDraw()
  }

  def copy(fg: Brush=fg, bg: Brush=bg): DrawingBoard = new DrawingBoard(w, h, fg, bg)(left, right)

  def diagonal: Vec = Vec(w, h)

  val focussedBrush: Brush      = Brushes.red(width=4, mode=GlyphShape.STROKE)
  val focussedFrame: GlyphShape = GlyphShape.rect(w-4, h-4)(focussedBrush)

  val selectBrush: Brush = Brushes("white.2.round.stroke")(mode=PaintMode.STROKE, blendMode=BlendMode.OVERLAY)

  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    surface.declareCurrentTransform(thisBoard)
    drawBackground(surface)

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
  val vertexPath = new GlyphShape.PathShape(Brushes("red.2.round.stroke.dashed(4)")(blendMode = BlendMode.DARKEN), false)


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
    surface.withOrigin(lastMouseDown-PathSymbols.last.centre) { PathSymbols.last.draw(surface)}
  }

  def addShape(kind: String, x:Scalar, y:Scalar)(shape: GlyphShape) : Unit = withState(s"+$kind"){
    val target = shape.targetLocatedAt(x min (w - shape.w), y min (h - shape.h))
    displayList.enqueue(target)
    selection = List(target)
  }

  def addText(text: String, brush: Brush): Unit = {
    var reselect: Seq[TargetShape] = Seq.empty
    val shape = GlyphShape.text (text) (brush)
    if (selection.nonEmpty) {
      val selected = selection.last
      selected.shape match {
        case t: Text =>
          lastMouseDown = selected.center+Vec(0, t.height*0.5f+t.drop*0.5f)
          reselect = selection
        case _ =>
      }
    }
    addShape ("text", shape)
    selection = reselect ++ List(selection.head)
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

class Dashboard(help: => Unit, hintSheet: StyleSheet, implicit val sheet: StyleSheet) {

    import sheet.{ex,em}
    val left, right = styled.ActiveString("   ")
    val done: Variable[Int]   = Variable(0){ case n => left.set(f"$n%03d") }
    val undone: Variable[Int] = Variable(0){ case n => right.set(f"$n%03d") }

    val drawingBoard = new DrawingBoard(1200, 800, lightGrey)(done, undone)

    var newBrush: Brush = Brushes("blue.0.stroke.round")

    def addButton(kind: String, dim: String="")(shape: => GlyphShape): Glyph = {
      val b  = styled.TextButton(kind){ _ => { drawingBoard.addShape(kind, shape); drawingBoard.feedback() } }(sheet)
      HintManager(b.asInstanceOf[Enterable], 5, ()=>s"Add a $dim", true)(hintSheet)
      b
    }



    def addText(text: String): Unit = {
      drawingBoard.addText(text, newBrush.copy)
      textField.text=""
    }


  class withRadius(R: Scalar)(shape: GlyphShape) extends GlyphShape {
      val delta = (Vec(2*R, 2*R)-shape.diagonal) * 0.5f

      def draw(surface: Surface): Unit = {
        surface.withOrigin(delta) { shape.draw(surface)}
      }

      def diagonal: Vec = Vec(2*R, 2*R)


      override def enclosing(point: Vec): Option[GlyphShape] = {
        val origin = point - delta
        if (shape.encloses(point-delta)) Some(this) else None
      }

    }


    val blackArrow =  arrow(black)

    var width, height, radius: Scalar = 150
    var scale: Scalar = 1.0f
    var vertices: Int = 3

    def RAD(shape: GlyphShape): GlyphShape = shape // new withRadius(radius)(shape)

    def rectangle(w: Scalar, h: Scalar)(brush: Brush): GlyphShape = {
      val path=new PathShape(brush, false)
      path.moveTo(0, 0)
      path.lineTo(w, 0)
      path.lineTo(w, h)
      path.lineTo(0, h)
      path.closePath
      path
    }

    val shapes: List[Glyph] = List(
      addButton("Rect", "rectangle (wxh)")(rectangle(width,height)(newBrush.copy).scale(scale max 1)),
      addButton("Circ", "circle (radius r)")(circle(radius)(newBrush.copy).scale(scale max 1)),
      addButton("Arrow", "arrow")(arrow(newBrush.copy).scale(scale max 0.3f)),
      addButton("Star", "v-pointed star size r")(RAD(GlyphShape.polygon(PolygonLibrary.regularStarPath(vertices, C=radius, R=radius).tail)(brush=newBrush.copy)).scale(scale max 0.3f)),
      addButton("Poly", "v-sided polygon size r")(RAD(GlyphShape.polygon(PolygonLibrary.regularPolygonPath(vertices, C=radius, R=radius).tail)(brush=newBrush.copy)).scale(scale max 0.3f)),
      //addButton("Text", "text from the text window")(GlyphShape.text(textField.text, sheet.textFont)(fg=newBrush.copy).scale(scale max 0.3f))
    )


  lazy val dimField: TextField = styled.TextField(onEnter = setDims, onCursorLeave = setDims, size = 50, initialText=s"w=$width, h=$height, r=$radius, scale=$scale, v=$vertices")
  lazy val textField: TextField = styled.TextField(size = 50, onEnter = addText(_), initialText=s"this is text")


    def setDims(specification: String): Unit = {
      @inline def isFloat(s: String):Boolean = s.matches("[0-9]+([.][0-9]+)?")
      val spec = specification.replaceAll("«[^»]+»","").strip()
      val specs = spec.toLowerCase.split("[ ]+|,[ ]*").toSeq
      for  { field <- specs }
        field match {
          case s"$v=$d" if v.nonEmpty && isFloat(d) =>
            val dim = d.toFloat
            v.head match {
              case 'w' => width = dim
              case 'h' => height = dim
              case 'r' => radius = dim
              case 's' => scale = dim
              case 'v' => vertices = dim.toInt
              case _ =>
                dimField.text=s"$spec «$field»"
                logging.Default.warn(s"$spec does not specify w=, h=, r=, s=, v=")
                return
            }

          case other =>
            dimField.text=s"$spec «$other»"
            logging.Default.warn(s"$spec does not specify as w=, h=, r=, s=, v=")
            return
        }

      dimField.text=s"w=$width, h=$height, r=$radius, scale=$scale, v=$vertices"
    }

    def HintedButton(label: String, hint: String="")(action: Reaction)(implicit style: StyleSheet): Glyph = {
      val button = styled.TextButton(label)(action)(style)
      if (hint.nonEmpty)  HintManager(button.asInstanceOf[Enterable], 5, ()=>hint)(hintSheet)
      button
    }

    def DynamicHintedButton(label: String, hint: ()=>String)(action: Reaction): Glyph = {
      val button = styled.TextButton(label)(action)
      HintManager(button.asInstanceOf[Enterable], 5, hint, false)(hintSheet)
      button
    }

    def brushError(error: NonBrush): Unit = {
      val message = styled.Label(s"${error.why}\n\n${Brushes.brushSpecificationNotation}\n", align=Left)
      OK(message).InFront(GUI).start()
    }

    val brushChooser = new BrushChooser(protoBrush=newBrush.copy, resultBrush = newBrush, onError = brushError(_) )(sheet)
    lazy val brushChooserWindow = brushChooser.Dialogue.InFront(GUI)

    lazy val GUI: Glyph = NaturalSize.Col(align=Center)(
      FixedSize.Row(width=drawingBoard.w, align=Mid)(
        HintedButton("Restart", "Clear the drawing and start again"){ _ => drawingBoard.restart() },
        HintedButton("Cleanup", "Preserve the drawing and start again"){ _ => drawingBoard.clean() },
        HintedButton("Brush", "Set the brush properties") { _ => if (brushChooserWindow.running.isEmpty) brushChooserWindow.start()},
        sheet.hFill(),
        HintedButton("Z", "Order the display to put smaller objects on top." )   { _ => {drawingBoard.zedOrder()}},
        sheet.hFill(1, 2f),
        HintedButton("Help"){ _ => help }
      ),
      drawingBoard,
      ex,
      FixedSize.Row(width=drawingBoard.w, align=Mid) (
        DynamicHintedButton("< ", drawingBoard.undoHint){ _ =>  drawingBoard.handle(drawingBoard.UndoGesture, Vec.Origin, Vec.Zero) },
        left.framed(),
        styled.Label(" "),
        right.framed(),
        DynamicHintedButton(">", drawingBoard.redoHint){ _ =>  drawingBoard.handle(drawingBoard.RedoGesture, Vec.Origin, Vec.Zero) },
        sheet.hFill(),


        HintedButton("o", "Compose selection in place")   { _ => {drawingBoard.composeSelectedInPlace()}},
        HintedButton("|", "Compose selection horizontally in selection order")   { _ => {drawingBoard.composeSelected(_.|||)}},
        HintedButton("-", "Compose selection vertically in selection order")   { _ => {drawingBoard.composeSelected(_.---)}},
        HintedButton("~", "Compose selection by superimposing in selection order")  { _ => {drawingBoard.composeSelected(_.~~~)}},
        HintedButton("↯", "Decompose the selected composition" )   { _ => {drawingBoard.decomposeSelected()}},
        sheet.hFill(),
        HintedButton("Clear", "Clear the path design") { _ => drawingBoard.fromButton("-vertices") { drawingBoard.vertices = Seq.empty } },
        HintedButton("Path", "Make a path from the path design"){ _ => drawingBoard.fromButton("path") { drawingBoard.addPoly(newBrush.copy); drawingBoard.vertices = Seq.empty }},
        sheet.hFill(),
        DynamicHintedButton("Paint", ()=>s"Repaint selection with  $newBrush"){ _ => drawingBoard.transformSelected({ shape => shape.withBrushes(newBrush.copy)}, "repaint")},
      ),
      ex,
      FixedSize.Row(width=drawingBoard.w, align=Mid)(shapes),
      ex,
      dimField.framed() beside em beside textField.framed()
    ).enlarged(20)
}

object Play extends Application {

  val LocalSheet: StyleSheet = StyleSheet()

  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=styles.decoration.RoundFramed(fg=blue(width=4, cap=PaintStrokeCap.ROUND), enlarge=0.25f, radius=0.25f),
    buttonFontSize = 20,
    labelFontSize = 20,
    textFontSize = 20,
    backgroundBrush = Brushes.white
  )

  def title = s"""Play"""
  override
  val defaultIconPath: Option[String] = Some ("PNG/WorcesterCrest.png")

  implicit val hintSheet: StyleSheet = interfaceStyle.copy(fontScale = 0.65f)
  val help: Glyph = {
    import glyphXML.Language._
    <div width="70em"  fontSize="16" align="justify" leftMargin="3em" rightMargin="3em">
      <p align="center">
        Play -- playing with little diagrams
      </p>
      <p>
        The diagram consists of a collection of geometric objects. Shapes are added to the diagram at the last-clicked position
        using one of the shape-labelled buttons or the <b>Path</b> button (<i>qv</i>). The default dimensions of the shapes are
        specified in an editable text field in the interface. The default brush used when a shape is added is
        specified in the <b>Palette</b> window.
      </p>
      <p>
        Some of the objects
        may be "live", and some "selected". Liveness is shown by the brightening of a small circular "liveness"
        indicator near their centre, and of the eight yellow "attachment points" at the corners
        and mid-edges of their bounding box.
      </p>
      <p>
        The first object is selected by clicking (within) it with the primary or secondary mouse button; subsequent objects
        are selected (or deselected if they are already selected) by clicking within them with the secondary mouse button.
      </p>
      <p>
        The first object selected has a bright circular "selection" indicator surrounding its "liveness" indicator;
        others are shown linked  to it, in the order in which they were selected, by white line-segments.
      </p>
      <p>
        The <b>Path</b> button, constructs a closed polygonal path from the <i>current path design</i>, to which points are added
        by clicking <b>Secondary-Shift</b> (or <b>Primary-Command-Shift</b> in the appropriate place. The path design is shown as a red dotted
        line starting with a red dotted X.
      </p>
      <p>
        When <i>v</i> is even the <b>Poly</b> button generates a <i>v</i>-edged regular polygon drawn as if inscribed in a concentric circle of radius <i>r</i>;
        when  <i>v</i> is odd the polygon is displaced slightly from the centre of the corresponding concentric circle.
      </p>
      <p>
        When <i>v</i> is odd the <b>Star</b> button generates a <i>v</i>-pointed star displaced slightly from the centre of the corresponding concentric circle.
        Its vertices are exactly those of the corresponding polygon.
      </p>
    </div>
  }


  lazy val GUI: Glyph =  new Dashboard({ styled.windowdialogues.Dialogue.OK(help)(hintSheet).InFront(GUI).start()}, hintSheet, interfaceStyle).GUI

}
