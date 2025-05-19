package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{BlendMode, PaintMode, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import io.github.humbleui.types.Rect
import org.sufrin.{glyph, logging}
import org.sufrin.glyph.Brushes.{black, blue, green, lightGrey, red, white, yellow}
import org.sufrin.glyph.GlyphShape.{arrow, circle, polygon, rect, FILL, PathShape, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet, GlyphButton}
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
    addCircle(0,0,7)
  }
}

/**
 * Container/editor for a displayList
 */
class Arena(background: Glyph)(left: Variable[Int], right: Variable[Int]) extends  GestureBasedReactiveGlyph {

  var lastMouse, lastMouseDown: Vec = background.diagonal scaled 0.5f

  val displayList: mutable.Queue[TargetShape] = new mutable.Queue[TargetShape]()

  var selection: Seq[TargetShape] = Seq.empty

  var vertices: Seq[Vec] = Seq.empty

  case class State (
                     lastMouse:      Vec,
                     lastMouseDown:  Vec,
                     vertices:       Seq[Vec],
                     displayList:    Seq[TargetShape],
                     selection:      Seq[TargetShape]
                   )


  def copyState: State = State(lastMouse, lastMouseDown, vertices, displayList.toSeq.map(_.copyState), selection.map(_.copyState))

  def restoreState(state: State): Unit = {
    this.lastMouse=state.lastMouse
    this.lastMouseDown=state.lastMouseDown
    this.vertices=state.vertices
    this.displayList.clear(); this.displayList.enqueueAll(state.displayList)
    this.selection=state.selection
  }

  var states, unstates: mutable.Stack[State] = new mutable.Stack[State]

  def withState(action: => Unit): Unit = {
    states.push(copyState)
    unstates.clear()
    action
  }

  def feedback(): Unit = {
    left.set(states.length)
    right.set(unstates.length)
  }

  def selected(location: Vec): Seq[TargetShape] = displayList.toSeq.filter(_.isBeneath(location))

  def hovered(location: Vec): Seq[TargetShape] = displayList.toSeq.filter(_.isBeneath(location))

  def restart(): Unit = {
    states.clear()
    unstates.clear()
    displayList.clear()
    selection = Seq.empty
    vertices=Seq.empty
    feedback()
  }

  def forUnselected(action: TargetShape => Unit): Unit =
    withState { for { shape <- displayList if !selection.contains(shape) } action(shape) }

  /** Apply the transform, as if about the current centre  */
  def transformSelected(transform: GlyphShape => GlyphShape): Unit = withState {
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
    if (selection.length>1) withState {
    var composed: GlyphShape = selection.head.shape
        for { v <- selection.tail } composed = compose(composed)(v.shape)
        val (x, y) = (selection.head.x, selection.head.y)
        displayList.dequeueAll(_.isIn(selection))
        val composite = new Composition(x, y, composed, selection)
        displayList.enqueue(composite)
        selection = List(composite)
      }
    else bell.play()

  def decomposeSelected(): Unit =
    if (selection.length == 1)
    selection.head match {
      case composition: Composition =>
        withState {
          displayList.dequeueAll(_.equals(selection.head))
          selection = composition.components
          displayList.enqueueAll(selection)
        }
      case _ => bell.play()
    } else bell.play()

  var deletion: Seq[TargetShape] = Seq.empty

  val bell = Sound.Clip("WAV/glass.wav")

  def redo() = {
    if (unstates.nonEmpty) {
      val state = unstates.pop()
      println(state)
      states.push(state)
      restoreState(state)
    } else bell.play()
  }

  def undo() = {
    if (states.nonEmpty) {
      val state = states.pop()
      unstates.push(state)
      restoreState(state)
    } else bell.play()
  }

  /** For external invocation of an action that requires feedback and redrawing */
  def fromButton(recordState: Boolean)(action: => Unit): Unit =
    { if (recordState) withState { action} else action
      feedback()
      reDraw()
    }

  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    // println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val PRESSED       = mods.includeAll(Pressed)
    val CONTROL       = mods.includeSome(Command|Control)
    val PRIMARY       = mods.includeAll(Primary   | Pressed)
    val SECONDARY     = mods.includeAll(Secondary | Pressed) || (PRIMARY && CONTROL)
    val COMPLEMENT    = mods.includeSome(Shift)
    val SHIFT         = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(this)
      case _: MouseLeaves => guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if CONTROL => transformSelected(_.scale(if (delta.x+delta.y>0) 1/1.05f else 1.05f))
      case _: MouseScroll            => transformSelected(_.turn((delta.x+delta.y).sign*5, COMPLEMENT))

      case Keystroke(key, _) if !PRESSED =>

      case Keystroke(key, _) if PRESSED =>
        key match {
          case Key.LEFT  =>  transformSelected(_.turn(-90, COMPLEMENT))
          case Key.RIGHT =>  transformSelected(_.turn(90, COMPLEMENT))

          case Key.MULTIPLY  =>  transformSelected(_.scale(1.05f))
          case Key.SLASH     =>  transformSelected(_.scale(1/1.05f))

          case Key.PERIOD =>  transformSelected(_.turn(5, COMPLEMENT))

          case Key.Z if CONTROL && SHIFT=> redo()


          case Key.Z if CONTROL => undo()

          case Key.A if CONTROL =>
            withState {
              if (COMPLEMENT)
                selection = displayList.toSeq.filter(_.notIn(selection))
              else
                selection = displayList.toSeq
            }

          case Key.HOME  => withState { selection = Seq.empty }

          case Key.X | Key.V =>
            addVertex(lastMouseDown)

          case Key.DELETE | Key.BACKSPACE =>
            withState {
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

      case MouseMove(_) =>
        if (PRESSED) {
          for { shape <- selection } shape.moveBy(delta.x, delta.y)
        }
        lastMouse = location

      case MouseClick(_) if (PRIMARY) => withState {
        selection = selected(location)
        lastMouseDown = location
        lastMouse = Vec.Origin
      }

      case MouseClick(_) if (SECONDARY) => withState  {
          val touched = selected(location)
          for {shape <- touched}
            if (selection contains shape)
              selection = selection.filterNot(_.eq(shape))
            else
              selection = selection ++ List(shape)

        lastMouse = Vec.Origin
      }

      case MouseClick(_)  if (PRESSED && COMPLEMENT) => withState {
          lastMouseDown = location
          addVertex(location)
      }

      case MouseClick(_) =>

    }
    feedback()
    reDraw()
  }

  def copy(fg: Brush=fg, bg: Brush=bg): Arena = new Arena(background)(left, right)

  val fg: Brush = background.fg
  val bg: Brush = background.bg

  def diagonal: Vec = background.diagonal

  val focussedBrush: Brush      = Brushes.red(width=4, mode=GlyphShape.STROKE)
  val focussedFrame: GlyphShape = GlyphShape.rect(background.w-4, background.h-4)(focussedBrush)

  val selectBrush: Brush = Brushes("white/2/ROUND")(mode=PaintMode.STROKE)


  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    surface.declareCurrentTransform(this)
    drawBackground(surface)
    background.draw(surface)

    for { shape <- displayList } {
      shape.setHovering(shape.isBeneath(lastMouse)||shape.canHandle(lastMouse))
      shape.setSelected(selection contains shape)
      shape.draw(surface)
    }

    indicateSelection(surface)
    indicateVertices(surface)

    if (guiRoot.hasKeyboardFocus(this)) focussedFrame.draw(surface)
  }

  val selectionPath = new GlyphShape.PathShape(selectBrush)
  val vertexPath = new GlyphShape.PathShape(Brushes("yellow/1.stroke-2-2")(blendMode = BlendMode.XOR))

  def indicateVertices(surface: Surface): Unit = if (vertices.nonEmpty) {
    vertexPath.reset()
    for { Vec(x,y) <- vertices.take(1) } vertexPath.moveTo(x,y)
    for { Vec(x,y) <- vertices.drop(1) } vertexPath.lineTo(x,y)
    for { Vec(x,y) <- vertices.take(1) } {
      vertexPath.lineTo(x,y)
      //vertexPath.addCircle(x, y, 10)
      vertexPath.addPathShape(PathSymbols.exx, x, y)
    }
    val b = (vertexPath.path.computeTightBounds())
    val w = b.getWidth
    val h = b.getHeight
    vertexPath.addPathShape(PathSymbols.pie, b.getLeft+w/2, b.getTop+h/2)
    vertexPath.draw(surface)
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
    selectionPath.addCircle(lastMouseDown.x, lastMouseDown.y, 5)
    selectionPath.draw(surface)
  }

  def addShape(x:Scalar, y:Scalar)(shape: GlyphShape) : Unit = withState {
    val target = shape.targetLocatedAt(x min (w - shape.w), y min (h - shape.h))
    displayList.enqueue(target)
    selection = List(target)
  }

  def addShape(shape: GlyphShape) : Unit = {
    val delta = shape.diagonal scaled 0.5f
    addShape(lastMouseDown.x-delta.x, lastMouseDown.y-delta.y)(shape)
  }

  def addVertex(location: Vec): Unit = {
    vertices = vertices++List(location)
    println(vertices.mkString(", "))
  }

  def addPoly(fg: Brush) : Unit = if (vertices.nonEmpty) withState {
    println(vertices)
    addShape(polygon(vertices)(fg))
    vertices = Seq.empty
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

  Page("Interact", ""){
    import sheet.{ex,em}
    val left, right = styled.ActiveString("   ")
    val done: Variable[Int]   = Variable(0){ case n => left.set(f"$n%03d") }
    val undone: Variable[Int] = Variable(0){ case n => right.set(f"$n%03d") }


    val arena = new Arena(rect(1200, 800)(lightGrey))(done, undone)

    def but(name: String)(shape: => GlyphShape): Glyph = {
      styled.TextButton(name){ _ => arena.fromButton(true) { arena.addShape(shape) } }
    }

    val blackArrow =  arrow(black)

    var newBrush: Brush = Brushes.blue()

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


    lazy val dimField: TextField = styled.TextField(onEnter = setDims, onCursorLeave = setDims, size = 50)


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
      newBrush = Brush.ofString(spec)
      println(newBrush)
    }

    val brushField = styled.TextField(onEnter=setNewBrush, onCursorLeave=setNewBrush, size=25)

    NaturalSize.Col(align=Center)(
      arena,
      ex,
      FixedSize.Row(width=arena.w, align=Mid)(
        styled.TextButton("Clear"){ _ => arena.restart() },
        sheet.hFill(),

        styled.TextButton("|")   { _ => arena.fromButton(true){arena.composeSelected(_.|||)}},
        styled.TextButton("-")   { _ => arena.fromButton(true){arena.composeSelected(_.---)}},
        styled.TextButton("~")   { _ => arena.fromButton(true){arena.composeSelected(_.~~~)}},
        styled.TextButton("↯")   { _ => arena.fromButton(false){arena.decomposeSelected()}},
        sheet.hFill(),
        styled.TextButton("xxx") { _ => arena.fromButton(true){ arena.vertices=Seq.empty} },
        styled.TextButton("Path"){ _ => arena.fromButton(true) { arena.addPoly(newBrush.copy) }},
        sheet.hFill(),
        styled.TextButton("Recolour"){ _ => arena.fromButton(true) { arena.transformSelected{ shape => shape.withForeground(newBrush.copy)}}},
        sheet.hFill(1, 2f),
        styled.TextButton("<"){ _ =>  arena.fromButton(false){arena.undo() } },
        left.framed(),
        styled.Label(" "),
        right.framed(), styled.TextButton(">"){_ => arena.fromButton(false){arena.redo() } },
        styled.TextButton("Help"){ _ =>}
      ),
      ex,
      FixedSize.Row(width=arena.w, align=Mid)(shapes),
      ex,
      styled.Label("Brush: ") beside brushField.framed() beside styled.Label(" ")  beside dimField.framed()
    ).enlarged(20)
  }
  //Page("Test", "")(new Page1(sheet).GUI)

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
