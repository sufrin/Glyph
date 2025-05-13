package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{PaintMode, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import org.sufrin.glyph
import org.sufrin.glyph.Brushes.{black, blue, green, lightGrey, red, white, yellow}
import org.sufrin.glyph.GlyphShape.{arrow, circle, rect, FILL, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet, GlyphButton}

import java.io.File
import scala.collection.mutable




/**
 * Container/editor for a displayList
 */
class Arena(background: Glyph)(left: Variable[Int], right: Variable[Int]) extends  GestureBasedReactiveGlyph {

  var lastMouse, lastMouseDown: Vec = Vec(-1,-1)

  val displayList: mutable.Queue[GlyphVariable] = new mutable.Queue[GlyphVariable]()

  var selection: Seq[GlyphVariable] = Seq.empty

  case class State (
                     lastMouse:      Vec,
                     lastMouseDown:  Vec,
                     displayList:    Seq[GlyphVariable],
                     selection:      Seq[GlyphVariable]
                   )


  def copyState: State = State(lastMouse, lastMouseDown, displayList.toSeq.map(_.copyState), selection.map(_.copyState))

  def restoreState(state: State): Unit = {
    this.lastMouse=state.lastMouse
    this.lastMouseDown=state.lastMouseDown
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

  def selected(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.canHandle(location))

  def hovered(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.isBeneath(location))

  def restart(): Unit = {
    states.clear()
    unstates.clear()
    displayList.clear()
    selection = Seq.empty
  }

  def forUnselected(action: GlyphVariable => Unit): Unit =
    withState { for { shape <- displayList if !selection.contains(shape) } action(shape) }

  /** Apply the transform, as if about the current centre  */
  def transformSelected(transform: GlyphShape => GlyphShape): Unit = withState {
    deletion = selection
    val mapped = selection.map {
      case v: GlyphVariable =>
        val v$ = transform(v.shape)
        val delta = v.diagonal - v$.diagonal
        v$.variable(v.x+delta.x/2, v.y+delta.y/2)
    }
    displayList.dequeueAll(_.isIn(selection))
    deletion = selection
    displayList.enqueueAll(mapped)
    selection = mapped
  }

  /** compose the selected elements in the order of selection  */
  def composeSelected(compose: GlyphShape => GlyphShape => GlyphShape): Unit = withState {
    var composed: GlyphShape = selection.head.shape
    for { v <- selection.tail } composed = compose(composed)(v.shape)
    val (x, y) = (selection.head.x, selection.head.y)
    displayList.dequeueAll(_.isIn(selection))
    components = selection
    val composite = (composed.variable(x, y))
    displayList.enqueue(composite)
    selection = List(composite)
  }

  var deletion, components: Seq[GlyphVariable] = Seq.empty

  val bell = Sound.Clip("WAV/glass.wav")

  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    // println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val ACT        = mods.includeAll(Pressed)
    val REL        = mods.includeAll(Released)
    val CTL        = mods.includeSome(Command|Control|Secondary)
    val PRIMARY    = mods.are(Primary   | Pressed)
    val SECONDARY  = mods.are(Secondary | Pressed)
    val COMPLEMENT = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(this)
      case _: MouseLeaves => guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if CTL => transformSelected(_.scale(if (delta.x+delta.y>0) 1/1.05f else 1.05f))
      case _: MouseScroll        => transformSelected(_.turn((delta.x+delta.y).sign*5, COMPLEMENT))

      case Keystroke(key, _) =>
        key match {
          case Key.LEFT  if ACT => transformSelected(_.turn(-90, COMPLEMENT))
          case Key.RIGHT if ACT => transformSelected(_.turn(90, COMPLEMENT))

          case Key.MULTIPLY  if ACT => transformSelected(_.scale(1.05f))
          case Key.SLASH     if ACT => transformSelected(_.scale(1/1.05f))

          case Key.PERIOD if ACT => transformSelected(_.turn(5, COMPLEMENT))

          case Key.Z if ACT && CTL && mods.includeSome(Shift)=>
              if (unstates.nonEmpty) {
                val state = unstates.pop()
                states.push(state)
                restoreState(state)
              }

          case Key.Z if ACT && CTL =>
            if (states.nonEmpty) {
              val state = states.pop()
              unstates.push(state)
              restoreState(state)
            }

          case Key.A if ACT && CTL =>
            withState {
              if (COMPLEMENT)
                selection = displayList.toSeq.filter(_.notIn(selection))
              else
                selection = displayList.toSeq
            }

          case Key.HOME   if ACT  =>
            withState { selection = Seq.empty }


          case Key.DELETE | Key.BACKSPACE if ACT =>
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

          case Key.H  if selection.length > 1 => composeSelected( _.||| )
          case Key.V  if selection.length > 1 => composeSelected( _.--- )
          case Key.S  if selection.length > 1 => composeSelected( _.~~~ )
          case Key.D  if components.nonEmpty =>
            withState {
              displayList.dequeueAll(_.isIn(selection))
              deletion = selection
              displayList.enqueueAll(components)
              selection = components
              components = Seq.empty
            }

          // ignore shift buttons
          case Key.SHIFT | Key.CONTROL | Key.CAPS_LOCK | Key.ALT | Key.MAC_COMMAND | Key.MAC_FN | Key.MAC_OPTION =>

          case _ =>
            if (mods.includeSome(Pressed)) bell.play()

        }

      case MouseMove(_) =>
        if (ACT) {
          for { shape <- selection } shape.moveBy(delta.x, delta.y)
        }
        lastMouse = location

      case MouseClick(_) if (SECONDARY) => withState { selection = selected(location) }

      case MouseClick(_) if (PRIMARY)   =>
        withState  {
          val touched = selected(location)
          for {shape <- touched}
            if (selection contains shape)
              selection = selection.filterNot(_.eq(shape))
            else
              selection = selection ++ List(shape)

        }

      case MouseClick(_) =>
        if (COMPLEMENT) lastMouseDown=location else lastMouseDown=Vec.Origin

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

    if (guiRoot.hasKeyboardFocus(this)) focussedFrame.draw(surface)
  }

  val selectionPath = new GlyphShape.PathShape(selectBrush)

  def indicateSelection(surface: Surface): Unit = {
    selectionPath.reset()
    selection.length match
    { case 0 =>
      case 1 =>
        for { shape <- selection.take(1) } {
          selectionPath.addCircle(shape.x+shape.w/2, shape.y+shape.h/2, 10)
        }
      case _ =>
        val path =
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
    val v = shape.variable(x min (w - shape.w), y min (h - shape.h))
    displayList.enqueue(v)
    lastMouseDown=Vec.Zero
    feedback()
  }

  def addShape(shape: GlyphShape) : Unit = addShape(lastMouseDown.x, lastMouseDown.y)(shape)
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
    import sheet.ex
    val left, right = styled.ActiveString("------------")
    val done: Variable[Int]   = Variable(0){ case n => left.set(f" $n%03d done") }
    val undone: Variable[Int] = Variable(0){ case n => right.set(f" $n%03d undone") }


    val arena = new Arena(rect(1200, 800)(lightGrey))(done, undone)

    def but(name: String)(shape: =>GlyphShape): Glyph = {
      styled.TextButton(name){ _ => arena.addShape(shape) }
    }

    val blackArrow =  arrow(black)

    var newBrush: Brush = Brushes.blue()

    val shapes = List(
      but("Sq")(rect(150,150)(newBrush.copy)),
      but("Circ")(circle(75)(newBrush.copy)),
      but("Arrow")(arrow(newBrush.copy)),
      but("Star")(PolygonLibrary.star7(fg=newBrush.copy))
    )

    val colours = styled.RadioCheckBoxes(List("R","G","B", "Y", "Wh", "Bl"), "Bl") {
      case None =>
      case Some(n) => n match {
            case 0 => newBrush.setColor(red.color)
            case 1 => newBrush.setColor(green.color)
            case 2 => newBrush.setColor(blue.color)
            case 3 => newBrush.setColor(yellow.color)
            case 4 => newBrush.setColor(white.color)
            case 5 => newBrush.setColor(black.color)
          }
    }

    val widths = styled.RadioCheckBoxes(List("0","1","2", "4", "8", "16"), "0") {
      case None    =>
      case Some(i) =>
        var w = 1
        for { _ <- 0 until i } w=w*2
        newBrush.strokeWidth(w.toFloat)
    }

    val modes = styled.RadioCheckBoxes(List("Fill", "Stroke")){
      case Some(0) =>  newBrush.setMode(FILL)
      case Some(1) =>  newBrush.setMode(STROKE)
      case _ =>
    }

    val decor = styled.RadioCheckBoxes(List("--", "~~")){
      case Some(0) =>  newBrush.setPathEffect(Brushes.white.pathEffect)
      case Some(1) =>  newBrush.setPathEffect(Brush.ofString("white~3~3").pathEffect)
      case _ =>
    }

    def setNewBrush(spec: String): Unit = {
      newBrush = Brush.ofString(spec)
      println(newBrush)
    }

    val brushField = styled.TextField(onEnter=setNewBrush, onCursorLeave=setNewBrush, size=50)

    NaturalSize.Col(align=Center)(
      arena,
      ex,
      FixedSize.Row(width=arena.w, align=Mid)(
        styled.TextButton("Restart"){ _ => arena.restart() },
        sheet.hFill(),
        left.framed(), right.framed(),
        styled.TextButton("Help"){ _ =>}
      ),
      ex,
      FixedSize.Row(width=arena.w, align=Mid)(shapes),
      ex,
      brushField.framed(),
//      FixedSize.Row(width=arena.w, align=Mid)(
//        colours.arrangedHorizontally(),
//        widths.arrangedHorizontally(),
//        modes.arrangedVertically(),
//        decor.arrangedVertically()
//      )
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
