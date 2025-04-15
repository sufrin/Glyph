package org.sufrin.glyph
package tests

import styled.BookSheet

import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove, MouseButton}
import GlyphTypes.{EventKey, Scalar, Window}

import org.sufrin.glyph.Brushes.blue
import org.sufrin.glyph.tests.DocumentationDiagrams.white

import scala.collection.mutable

object Games extends Application  {
  import styles._

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

  def title = s"""Games"""

  override
  val defaultIconPath: Option[String] = Some ("PNG/WorcesterCrest.png")

}

class Arena(background: Glyph) extends ReactiveGlyph {

  val displayList: mutable.Queue[GlyphAt] = new mutable.Queue[GlyphAt]()

  def selected(location: Vec): Seq[GlyphAt] = displayList.toSeq.filter(_.contains(location))

  var selection: Seq[GlyphAt] = Seq.empty

  override def accept(key: EventKey, location: Vec, window: Window): Unit = {
    reDraw()
  }

  override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = {
    import Modifiers._
    if (event.include(Primary|Pressed) && selection.length==1) selection(0).moveTo(location.x-selection(0).w/2, location.y-selection(0).h/2)
    reDraw()
  }

  override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = {
    import Modifiers._
    if (event.include(Primary|Pressed)) selection=selected(location) else selection = Seq.empty
    reDraw()
  }


  def copy(fg: Brush=fg, bg: Brush=bg): Arena = new Arena(background)

  val fg: Brush = background.fg
  val bg: Brush = background.bg

  def diagonal: Vec = background.diagonal

  val aliveBrush: Brush = Brushes.red(width=4, mode=GlyphShape.STROKE)
  val alive: GlyphShape = GlyphShape.rect(background.w-4, background.h-4)(aliveBrush)

  val selectString: Brush = Brushes("white/5/ROUND~5~4")

  override def accept(event: GlyphEvent, location: Vec, window: Window): Unit = event match {
    case _: GlyphEnter =>
      guiRoot.grabKeyboard(this)
      reDraw()
    case _: GlyphLeave =>
      guiRoot.freeKeyboard(completely = true)
      reDraw()
  }

  val blob = GlyphShape.circle(6)(Brushes("blue/2.stroke"))

  def draw(surface: Surface): Unit = surface.withClip(diagonal) {
    surface.declareCurrentTransform(this)
    drawBackground(surface)
    background.draw(surface)
    for { shape <- displayList } shape.draw(surface)
    for { shape <- selection } GlyphShape.line(Vec(0,h), Vec(shape.x, shape.y)+(shape.diagonal scaled 0.5f))(selectString).draw(surface)
    if (guiRoot.hasKeyboardFocus(this)) alive.draw(surface)
  }

}

class Game1(sheet: StyleSheet) {
  implicit val style: StyleSheet=sheet
  import glyphXML.Language.translation._
  import GlyphShape._
  import Brushes._
  import style.{ex,em}

  def thing(r: Scalar, brusha: Brush, brushb: Brush): GlyphShape = {
    import GlyphShape._
    rect(10, 10)(brusha(mode=FILL,cap=ROUND)) ~~~ circle(r)(brushb(mode=STROKE))
  }

  val arena = new Arena(rect(600, 500)(grey1))
  def randx = Math.random.toFloat*(arena.w-50)
  def randy = Math.random.toFloat*(arena.h-50)

  val shapes = arena.displayList
  locally {
    for { i<-1 to 15 } shapes.enqueue(thing(50, red(width=5), blue(width=5)).at(randx, randy))
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