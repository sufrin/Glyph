package org.sufrin.glyph
import GlyphTypes._

import io.github.humbleui.jwm.Screen

object Location {

  trait Location {
    /** Position in logical space */
    def logicalLocation: (Int, Int)

    /** The logical-to-pixel multiplier for this screen */
    def hardwareScale: Scalar // = screen.getScale // TODO: redundant?
    def softwareScale: Scalar // = overall scaling // TODO: redundant?
    def screen: Screen        // TODO: redundant?
    def effectiveScale: Scalar = hardwareScale*softwareScale
    // Screen coordinates in physical units
    def contentLocation: Pixels
    def reDraw(): Unit
  }

  def South(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w / 2, glyph.h))

  def SouthFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec((glyph.w-placed.w) / 2, glyph.h))

  def East(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w, glyph.h / 2))

  def EastFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w, (glyph.h-placed.h) / 2))

  def SouthEast(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w, glyph.h))

  def NorthEast(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w, 0f))

  def SouthWestFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(-placed.w, glyph.h))

  def WestFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(-placed.w, (glyph.h - placed.h) / 2))

  def NorthFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec((glyph.w - placed.w)/ 2, -placed.h))

  def NorthWestFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(-placed.w, -placed.h))

  def NorthEastFor(placed: Glyph)(glyph: Glyph): RelativeTo = RelativeTo(glyph, Vec(glyph.w, -placed.h))

  def OnRootOf(glyph: Glyph)(x: Scalar, y: Scalar): RelativeTo = RelativeTo(glyph.guiRoot, Vec(x, y))


  /** At a location in logical space relative to the location of the glyph */
  case class RelativeTo(glyph: Glyph, offset: Vec = Vec.Zero) extends Location {
    def rootDistance:    Vec      = glyph.rootDistance

    def reDraw(): Unit = glyph.guiRoot.reDraw()

    def logicalLocation: (Int, Int) = {
      val result = Glyph.logicalLocation(glyph, offset)
      result
    }

    def contentLocation: Pixels   = glyph.guiRoot.contentLocation
    def hardwareScale:   Scalar   = glyph.guiRoot.hardwareScale
    def softwareScale:   Scalar   = glyph.guiRoot.softwareScale
    def screen:          Screen   = glyph.guiRoot.currentScreen
  }

  /**
   * At an absolute location in logical space on (what is supposed to be) the
   * screen showing `showing`.
   * TODO: it currently shows on the principal screen
   */
  case class OnScreen(showing: Glyph)(x: Scalar, y: Scalar, val hardwareScale: Scalar = showing.guiRoot.hardwareScale) extends Location {
    def reDraw(): Unit              = showing.guiRoot.reDraw()
    def logicalLocation: Pixels     = Glyph.logicalLocation(Vec(x, y))
    def screen: Screen              = showing.guiRoot.eventHandler.screen
    def softwareScale: Scalar       = showing.guiRoot.softwareScale
    def contentLocation: Pixels     = (0,0)
  }

}
