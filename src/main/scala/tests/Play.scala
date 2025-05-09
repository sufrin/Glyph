package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{PaintMode, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import org.sufrin.glyph.Brushes.blue
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}
import org.sufrin.glyph.styled.{Book, BookSheet}

import scala.collection.mutable



/**
 * Container/editor for a displayList
 */
class Arena(background: Glyph) extends  GestureBasedReactiveGlyph {

  var lastMouse: Vec = Vec(-1,-1)

  val displayList: mutable.Queue[GlyphVariable] = new mutable.Queue[GlyphVariable]()

  def selected(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.handles(location))

  def hovered(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.beneath(location))

  var selection: Seq[GlyphVariable] = Seq.empty

  def forUnselected(action: GlyphVariable => Unit): Unit =
    for { shape <- displayList if !selection.contains(shape) } action(shape)

  def transformSelected(transform: GlyphShape => GlyphShape): Unit = {
    val mapped = selection.map{
      case v: GlyphVariable => transform(v.shape).variable(v.x, v.y)
    }
    displayList.dequeueAll(_.isIn(selection))
    displayList.enqueueAll(mapped)
    selection = mapped
  }

  var deletion: Seq[GlyphVariable] = Seq.empty

  def handle(gesture: Gesture, location: Vec, delta: Vec): Unit = {
    // println(gesture, location, delta)
    val mods: Bitmap  = gesture.modifiers
    val ACT        = mods.includeAll(Pressed)
    val CTL        = mods.includeSome(Command|Control|Secondary)
    val PRIMARY    = mods.are(Primary | Pressed)
    val COMPLEMENT = mods.includeSome(Shift)
    gesture match {
      case _: MouseEnters => guiRoot.grabKeyboard(this)
      case _: MouseLeaves =>  guiRoot.freeKeyboard(completely = true)

      case _: MouseScroll if CTL => transformSelected(_.turn(delta.y.sign*5))
      case _: MouseScroll  => for { shape <- selection } shape.moveBy(delta.x, delta.y)

      case Keystroke(key, _) =>
        key match {
          case Key.LEFT  if ACT => transformSelected(_.turn(-90))
          case Key.RIGHT if ACT => transformSelected(_.turn(90))

          case Key.MULTIPLY  if ACT => transformSelected(_.scale(1.05f))
          case Key.SLASH     if ACT => transformSelected(_.scale(1/1.05f))

          //case Key.PERIOD if ACT && COMPLEMENT => for {shape <- selection} shape.turnBy(-5)
          //case Key.PERIOD if ACT => for {shape <- selection} shape.turnBy(5)

          case Key.Z if ACT =>
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
       if (PRIMARY)
          selection = selected(location)
        else {
        if (ACT && CTL)  {
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

  Page("Interact", "")(new ArenaWithShapes{
    import Brushes._
    import GlyphShape._
    def shapes(): Unit = {
      val blackArrow =  arrow(black)
      shape(100, 100)(rect(50,50)(red)|||rect(50,50)(blue))
      shape(300, 100)(superimposed(rect(blackArrow.w, blackArrow.h)(red), blackArrow))
      shape(500, 100)(superimposed(rect(blackArrow.w, blackArrow.h)(red).scale(2), blackArrow|||blackArrow.turn(180)))
      shape(600, 600)(circle(50)(blue))

    }
  }.GUI)
  //Page("Test", "")(new Page1(sheet).GUI)

  val GUI: Glyph =  {
    import glyphXML.Language.translation._
    NaturalSize.Col(align = Center)(
      <div width="70em" align="justify">
        <p align="center">GlyphShape Playground.</p>
      </div>,
      book.Layout.leftCheckBoxes(Justify),
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

  val GUI: Glyph = arena.framed()
}