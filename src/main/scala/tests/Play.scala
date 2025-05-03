package org.sufrin.glyph
package tests


import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, Key, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import io.github.humbleui.skija.PathFillMode
import org.sufrin.glyph.Modifiers.{Bitmap, Command, Control, Pressed, Released, Shift}

import java.awt.DisplayMode
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
class Arena(background: Glyph) extends ReactiveGlyph {

  val displayList: mutable.Queue[GlyphVariable] = new mutable.Queue[GlyphVariable]()

  def selected(location: Vec): Seq[GlyphVariable] = displayList.toSeq.filter(_.contains(location))

  var selection: Seq[GlyphVariable] = Seq.empty

  override def accept(key: EventKey, location: Vec, window: Window): Unit = {
    val mods: Bitmap  = key
    val ACT = mods.includeAll(Pressed)
    val CTL = mods.includeSome(Command|Control)
    key.getKey match {
      case Key.LEFT   if ACT => for { shape <- selection } shape.turnBy(-90)
      case Key.RIGHT  if ACT => for { shape <- selection } shape.turnBy(90)
      case Key.UP     if ACT => for { shape <- selection } shape.turnBy(-5)
      case Key.DOWN   if ACT => for { shape <- selection } shape.turnBy(5)

      case Key.A      if ACT && CTL => selection=displayList.toSeq

      case Key.HOME   if ACT && CTL =>
           for { shape <- displayList } shape.turnTo(0)
           selection = Seq.empty

      case Key.HOME   if ACT =>
           for { shape <- selection } shape.turnTo(0)
           selection = Seq.empty

      case Key.DELETE | Key.BACKSPACE if ACT =>
           displayList.dequeueAll { shape => selection contains shape }
           selection = Seq.empty

      case _ =>
    }
    reDraw()
  }

  override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = {
    import Modifiers._
    val mods: Bitmap = event
    if (mods.includeAll(Primary|Pressed)) {
      for { shape <- selected(location) } shape.moveTo(location.x-shape.w/2, location.y-shape.h/2)
    }
    reDraw()
  }

  override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = {
    import Modifiers._
    val mods: Bitmap = event
    if  (mods.includeAll(Pressed) && (mods.includeSome(Control|Command) || mods.includeSome(Secondary))) {
      val touched = selected(location)
      for { shape <- touched }
        if (selection contains shape)
          selection = selection.filterNot{ s=>s==shape}
        else
          selection = selection++List(shape)
    }
    else
    if  (mods.are(Primary|Pressed)) selection=selected(location)
    reDraw()
  }

  def copy(fg: Brush=fg, bg: Brush=bg): Arena = new Arena(background)

  val fg: Brush = background.fg
  val bg: Brush = background.bg

  def diagonal: Vec = background.diagonal

  val aliveBrush: Brush = Brushes.red(width=4, mode=GlyphShape.STROKE)
  val alive: GlyphShape = GlyphShape.rect(background.w-4, background.h-4)(aliveBrush)

  val selectString: Brush = Brushes("white/2/ROUND~8~2")

  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = event match {
    case _: GlyphEnter =>
      guiRoot.grabKeyboard(this)
      reDraw()
    case _: GlyphLeave =>
      guiRoot.freeKeyboard(completely = true)
      reDraw()
  }
  
  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    surface.declareCurrentTransform(this)
    drawBackground(surface)
    background.draw(surface)
    for { shape <- displayList } shape.draw(surface)
    linkShapes(surface)
    if (guiRoot.hasKeyboardFocus(this)) alive.draw(surface)
  }

  def linkShapes(surface: Surface): Unit = {
    var loc = Vec(0, h)
    for { shape <- selection } {
      val nextLoc = Vec(shape.x, shape.y)+(shape.diagonal scaled 0.5f)
      GlyphShape.line(loc, nextLoc)(selectString).draw(surface)
      loc = nextLoc
    }
  }

}

class Game1(sheet: StyleSheet) {
  implicit val style: StyleSheet=sheet
  import glyphXML.Language.translation._
  import GlyphShape._
  import Brushes._
  import style.{ex,em}

  def blob = GlyphShape.arrow(Brushes("green/2"))

  def thing(r: Scalar, brusha: Brush, brushb: Brush): GlyphShape = {
    import GlyphShape._
    rect(10, 10)(brusha(mode = FILL, cap = ROUND)) ~~~ circle(r)(brushb(mode = STROKE)) ~~~ blob.scale(1.3f)
  }

  val arena = new Arena(rect(800, 500)(grey1))
  def randx = Math.random.toFloat*(arena.w-50)
  def randy = Math.random.toFloat*(arena.h-50)
  def randDegrees = Math.random.toFloat*(360f)
  def randScale = Math.random.toFloat*(2.5f)

  val path: PathShape = new PathShape(red(width=3, mode=STROKE))
  locally {
    path.fillMode(PathFillMode.EVEN_ODD)
    path.moveTo(100, 100)
    path.lineTo(150, 150)
    path.addRect(150, 150, 50, 60)
    path.addRect(200, 210, 50, 60)
  }

  val shapes: mutable.Queue[GlyphVariable] = arena.displayList

  locally {
    //shapes.enqueue(path.at(0, 0))
    val stroke = red(mode=STROKE)
    val rct: GlyphShape = arrow(red) ~~~ circle(arrow(red).w/1.7f)(stroke)
    for { i<-1 to 15 } shapes.enqueue(thing(50, red(width=5), blue(width=5)).variable(randx, randy))
    for { loc <- 1 to 5} shapes.enqueue(GlyphVariable(randx, randy, randDegrees, rct.scale(randScale)))
    shapes.enqueue(GlyphVariable(randx, randy, 0, blob.scale(3)))
    shapes.enqueue(GlyphVariable(randx, randy, 90, blob.scale(3)))
  }



  val GUI: Glyph = NaturalSize.Col(align=Center)(
    <div width="70em" align="justify">
      <p align="center">This is utterly inconsequential for the moment.</p>
    </div>,
    ex,
    arena.framed(),
    ex
  )
}