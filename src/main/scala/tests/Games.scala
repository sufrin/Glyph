package org.sufrin.glyph
package tests

import styled.BookSheet

import io.github.humbleui.jwm.{EventMouseButton, EventMouseMove}
import GlyphTypes.{EventKey, Window}

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

  override def accept(key: EventKey, location: Vec, window: Window): Unit = {
    if (key.isPressed) {
      for { g <- displayList } { g.dx += 20; g.dy+=30 }
    }
    reDraw()
  }

  override def accept(event: EventMouseButton, location: Vec, window: Window): Unit = super.accept(event, location, window)

  override def accept(event: EventMouseMove, location: Vec, window: Window): Unit = super.accept(event, location, window)


  def copy(fg: Brush=fg, bg: Brush=bg): Arena = new Arena(background)

  val fg: Brush = background.fg
  val bg: Brush = background.bg

  def diagonal: Vec = background.diagonal

  import GlyphShape.{FILL,STROKE}
  val aliveBrush: Brush = Brushes.red(width=4, mode=STROKE)
  val alive: GlyphShape = GlyphShape.rect(background.w-4, background.h-4)(aliveBrush)

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
    if (guiRoot.hasKeyboardFocus(this)) alive.draw(surface)
  }

}

class Game1(sheet: StyleSheet) {
  implicit val style: StyleSheet=sheet
  import glyphXML.Language.translation._
  import GlyphShape._
  import Brushes._
  import style.{ex,em}

  val arena = new Arena(rect(600, 500)(grey1))
  val shapes = arena.displayList
  locally {
    for { i<-1 to 6 } shapes.enqueue(circle(i*20f)(red(width=6f*i, mode=STROKE)).at(i*55f, i*55f))
  }


  val GUI: Glyph = NaturalSize.Col(align=Center)(
    <div width="70em" align="justify">
      <p align="center">This is a little experimental game.</p>
    </div>,
    ex,
    arena.framed(),
    ex
  )
}