package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{PaintMode, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import org.sufrin.glyph
import org.sufrin.glyph.Brushes.blue
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet}

import scala.collection.mutable



/**
 * Container/editor for a displayList
 */
class Arena(background: Glyph) extends  GestureBasedReactiveGlyph {

  var lastMouse, lastMouseDown: Vec = Vec(-1,-1)

  val displayList: mutable.Queue[GlyphVariable] = new mutable.Queue[GlyphVariable]()

  def selected(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.handles(location))

  def hovered(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.beneath(location))

  var selection: Seq[GlyphVariable] = Seq.empty

  def forUnselected(action: GlyphVariable => Unit): Unit =
    for { shape <- displayList if !selection.contains(shape) } action(shape)

  /** Apply the transform, as if about the current centre  */
  def transformSelected(transform: GlyphShape => GlyphShape): Unit = {
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

  /** compose the selected elements in their order of selection  */
  def composeSelected(compose: GlyphShape => GlyphShape => GlyphShape): Unit = {
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


  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    // println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val ACT        = mods.includeAll(Pressed)
    val CTL        = mods.includeSome(Command|Control|Secondary)
    val PRIMARY    = mods.are(Primary   | Pressed)
    val SECONDARY  = mods.are(Secondary | Pressed)
    val COMPLEMENT = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(this)
      case _: MouseLeaves =>  guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if CTL => transformSelected(_.scale(if (delta.x+delta.y>0) 1/1.05f else 1.05f))
      case _: MouseScroll => transformSelected(_.turn((delta.x+delta.y).sign*5, COMPLEMENT))

      case Keystroke(key, _) =>
        key match {
          case Key.LEFT  if ACT => transformSelected(_.turn(-90, COMPLEMENT))
          case Key.RIGHT if ACT => transformSelected(_.turn(90, COMPLEMENT))

          case Key.MULTIPLY  if ACT => transformSelected(_.scale(1.05f))
          case Key.SLASH     if ACT => transformSelected(_.scale(1/1.05f))

          case Key.PERIOD if ACT => transformSelected(_.turn(5, COMPLEMENT))

          case Key.Z if CTL && ACT =>
              for { shape <- deletion } displayList.enqueue(shape)
              deletion = Seq.empty

          case Key.A if ACT && CTL =>
              if (COMPLEMENT)
                selection = displayList.toSeq.filter(_.notIn(selection))
              else
                selection = displayList.toSeq


          case Key.HOME   if ACT && CTL =>
            //for { shape <- displayList } shape.turnTo(0)
            selection = Seq.empty

          case Key.HOME   if ACT =>
            //for { shape <- selection } shape.turnTo(0)
            selection = Seq.empty

          case Key.DELETE | Key.BACKSPACE if ACT =>
              if (COMPLEMENT) {
                deletion = displayList.toSeq.filter(_.notIn(selection))
                displayList.dequeueAll(_.notIn(selection))
              } else {
                displayList.dequeueAll(_.isIn(selection))
                deletion = selection
              }
            selection = Seq.empty

          case Key.H  if selection.length > 1 => composeSelected( _.||| )
          case Key.V  if selection.length > 1 => composeSelected( _.--- )
          case Key.S  if selection.length > 1 => composeSelected( _.~~~ )
          case Key.D  if components.nonEmpty =>
            displayList.dequeueAll(_.isIn(selection))
            deletion = selection
            displayList.enqueueAll(components)
            selection = components
            components = Seq.empty

          case Key.SPACE =>
            if (ACT) println(deletion)

          case _ =>
        }

      case MouseMove(_) =>
        if (ACT) {
          for { shape <- selection } shape.moveBy(delta.x, delta.y)
        }
        lastMouse = location

      case MouseClick(_) =>
       if (ACT) lastMouseDown=location
       if (PRIMARY)
          selection = selected(location)
        else {
        if (SECONDARY || (ACT && CTL))  {
          val touched = selected(location)
          for {shape <- touched}
            if (selection contains shape)
              selection = selection.filterNot(_.eq(shape))
            else
              selection = selection ++ List(shape)
        }
        lastMouse = location
      }
    }
    reDraw()
  }

  def copy(fg: Brush=fg, bg: Brush=bg): Arena = new Arena(background)

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
      shape.setHovering(shape.beneath(lastMouse))
      shape.setSelected(selection contains shape)
      shape.draw(surface)
    }

    linkShapes(surface)
    if (guiRoot.hasKeyboardFocus(this)) focussedFrame.draw(surface)
  }

  def linkShapes(surface: Surface): Unit = {
    selection.length match
    { case 0 =>
      case 1 =>
          //for { shape <- selection } shape.setSelected(true)
      case _ =>
        val path = new GlyphShape.PathShape(selectBrush)
        for { shape <- selection.take(1) } path.moveTo(shape.x+shape.w/2, shape.y+shape.h/2)
        for { shape <- selection.drop(1) } path.lineTo(shape.x+shape.w/2, shape.y+shape.h/2)
        for { shape <- selection.take(1) } path.lineTo(shape.x+shape.w/2, shape.y+shape.h/2)
        path.draw(surface)
    }
  }

}


object Play extends Application {

  /**
   * Default sheet
   */
  val LocalSheet: StyleSheet = StyleSheet()
  val interfaceStyle: StyleSheet = LocalSheet.copy(
    buttonDecoration=styles.decoration.Blurred(fg=Brushes.blue, blur=5, spread=5, delta=5),
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
    val arena = new ArenaWithShapes {
        import Brushes._
        import GlyphShape._
        def shapes(): Unit = {
          val blackArrow =  arrow(black)
          shape(100, 100)(rect(50,50)(red)|||rect(50,50)(blue))
          shape(300, 100)(superimposed(rect(blackArrow.w, blackArrow.h)(red), blackArrow))
          shape(300, 100)(PolygonLibrary.star7())
          shape(500, 100)(superimposed(rect(blackArrow.w, blackArrow.h)(red).scale(2), blackArrow|||blackArrow.turn(180)))
          shape(600, 600)(circle(50)(blue))
          shape(600, 300)(arrow(red))
          shape(600, 400)(arrow(blue(width=6, mode=STROKE)))
        }
    }
    NaturalSize.Col(align=Center)(
      arena.GUI,
      FixedSize.Row(width=arena.GUI.w, align=Mid)(
        // styled.TextButton("Unrotate selection"){ _ => arena.resetSelected() },
        styled.TextButton("Restart"){ _ => arena.resetShapes() },
        sheet.hFill(),
        styled.TextButton("Help"){ _ =>}
      )
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

abstract class ArenaWithShapes(implicit val sheet: StyleSheet) {
  import GlyphShape._
  import Brushes._

  def shapes(): Unit

  val pie = GlyphShape.pie(arrow(red).w/2)(red,green,blue)

  def thing(r: Scalar, brusha: Brush, brushb: Brush): GlyphShape = {
    import GlyphShape._
    rect(10, 10)(brusha(mode = FILL, cap = ROUND)) ~~~ circle(r)(brushb(mode = STROKE)) ~~~ arrow(red).scale(1.3f)
  }

  val arena = new Arena(rect(1200, 800)(lightGrey))

  def shape(shape: GlyphShape): Unit = {
      val v = shape.variable(randx min (arena.w - shape.w), randy min (arena.h - shape.h))
      arena.displayList.enqueue(v)
  }

  def shape(x:Scalar, y:Scalar)(shape: GlyphShape) : Unit = {
    val v = shape.variable(x min (arena.w - shape.w), y min (arena.h - shape.h))
    arena.displayList.enqueue(v)
  }

  def randx = Math.random.toFloat*(arena.w-56)
  def randy = Math.random.toFloat*(arena.h-56)
  def randDegrees = Math.random.toFloat*(360f)
  def randScale = Math.random.toFloat*(2.5f)

  locally {
    shapes()
  }

  def resetShapes(): Unit   = { arena.displayList.clear(); shapes() }

  val GUI: Glyph = arena.framed()
}