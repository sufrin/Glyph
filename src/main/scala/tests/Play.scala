package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.{PaintMode, PathFillMode}
import org.sufrin.glyph.Modifiers.{Alt, Bitmap, Command, Control, Pressed, Primary, Released, Secondary, Shift}
import gesture._

import org.sufrin.glyph.Brushes.blue
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}

import scala.collection.mutable

/**
 * Trial arena for gestures
 */

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
  lazy val GUI = new Game1(interfaceStyle).GUI

  def title = s"""Play"""

  override
  val defaultIconPath: Option[String] = Some ("PNG/WorcesterCrest.png")

}


/**
 * Container/editor for a displayList
 */
class Arena(background: Glyph) extends  GestureBasedReactiveGlyph {

  var lastMouse: Vec = Vec(-1,-1)

  val displayList: mutable.Queue[GlyphVariable] = new mutable.Queue[GlyphVariable]()

  def selected(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.handles(location))

  def hovered(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.beneath(location))

  var selection: Seq[GlyphVariable] = Seq.empty

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

      case _: MouseScroll if CTL => for { shape <- selection } shape.turnBy(delta.y/10)
      case _: MouseScroll  => for { shape <- selection } shape.moveBy(delta.x, delta.y)

      case Keystroke(key, _) =>
        key match {
          case Key.LEFT if ACT => for {shape <- selection} shape.turnBy(-90)
          case Key.RIGHT if ACT => for {shape <- selection} shape.turnBy(90)

          case Key.PERIOD if ACT && COMPLEMENT => for {shape <- selection} shape.turnBy(-5)
          case Key.PERIOD if ACT => for {shape <- selection} shape.turnBy(5)

          case Key.Z if ACT =>
              for { shape <- deletion } displayList.enqueue(shape)
              deletion = Seq.empty

          case Key.A if ACT && CTL =>
              if (COMPLEMENT)
                selection = displayList.toSeq.filter(_.notIn(selection))
              else
                selection = displayList.toSeq


          case Key.HOME   if ACT && CTL =>
            for { shape <- displayList } shape.turnTo(0)
            selection = Seq.empty

          case Key.HOME   if ACT =>
            for { shape <- selection } shape.turnTo(0)
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
              selection = selection.filterNot {
                _.eq(shape)
              }
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
      shape.setSelected(selection.length==1 && (shape eq selection.head))
      shape.draw(surface)
    }
    showShapes(surface)
    //linkShapes(surface)
    if (guiRoot.hasKeyboardFocus(this)) focussedFrame.draw(surface)
  }

  def linkShapes(surface: Surface): Unit = if (selection.length>1) {
    val path = new GlyphShape.PathShape(selectBrush)
    //path.moveTo(0,0) // force the origin
    for { shape <- selection.take(1) } path.moveTo(shape.x+shape.w/2, shape.y+shape.h/2)
    for { shape <- selection.drop(1) } path.lineTo(shape.x+shape.w/2, shape.y+shape.h/2)
    for { shape <- selection.take(1) } path.lineTo(shape.x+shape.w/2, shape.y+shape.h/2) // close the path
    path.draw(surface)
  }

  def showShapes(surface: Surface): Unit =  if (selection.nonEmpty) {
    val path = new GlyphShape.PathShape(selection.head.handleBrush0(mode=FILL))
    path.moveTo(0, 0)
    for { shape <- selection } {
      val d=shape.diagonal
      path.path.addPath(shape.handle.path, shape.x+d.x/2, shape.y+d.y/2)
    }
    path.draw(surface)
  }

}

class Game1(sheet: StyleSheet) {
  implicit val style: StyleSheet=sheet
  import GlyphShape._
  import Brushes._
  import style.{ex,em}

  val pie = GlyphShape.pie(arrow(red).w/2)(red,green,blue)

  def thing(r: Scalar, brusha: Brush, brushb: Brush): GlyphShape = {
    import GlyphShape._
    rect(10, 10)(brusha(mode = FILL, cap = ROUND)) ~~~ circle(r)(brushb(mode = STROKE)) ~~~ arrow(red).scale(1.3f)
  }


  val arena = new Arena(rect(1200, 800)(lightGrey))
  def randx = Math.random.toFloat*(arena.w-56)
  def randy = Math.random.toFloat*(arena.h-56)
  def randDegrees = Math.random.toFloat*(360f)
  def randScale = Math.random.toFloat*(2.5f)

  locally {
    val shapes: mutable.Queue[GlyphVariable] = arena.displayList
    def add(g: GlyphShape): Unit = {
      val v = g.variable(randx min (arena.w - g.w), randy min (arena.h - g.h), randDegrees)
      shapes.enqueue(v)
    }
    val arrow = {
      val a = GlyphShape.arrow(blue(width=4, mode=STROKE, cap=ROUND))
      a~~~circle((a.w max a.h)/2)(invisible)
    }
    val boxed = {
      val a = GlyphShape.arrow(red(width=4, mode=STROKE, cap=ROUND))
      (a~~~a.turned(90)).scale(4)
    }
    if (true) {
      for {i <- 1 to 5} add(thing(30, red(width = 5), blue(width = 5)))
      for {loc <- 1 to 5} add(pie)
      add(arrow)
      add(arrow)
      add(arrow)
      add(boxed)
    } else {
      shapes.enqueue((rect(50,50)(red)|||rect(50,50)(blue)).variable(150, 100))
      shapes.enqueue((rect(50,50)(red)|||rect(50,50)(blue)).variable(100, 100))
      shapes.enqueue(arrow(blue).variable(0,0))
    }
  }


  val GUI: Glyph = {
    import glyphXML.Language.translation._
    NaturalSize.Col(align = Center)(
      <div width="70em" align="justify">
        <p align="center">This is utterly inconsequential for the moment.</p>
      </div>,
      ex,
      arena.framed(),
      ex,
    )
  }
}