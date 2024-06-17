package org.sufrin.glyph

/**
 * A convenience trait that defines several brushes.
 *
 * TODO: these should be systematically named and featured. The range of
 *       widths, and caps is unnecessary.
 */
trait Brushes {

  import io.github.humbleui.skija.PaintStrokeCap
  val SQUARE     = PaintStrokeCap.SQUARE
  val ROUND      = PaintStrokeCap.ROUND
  val BUTT       = PaintStrokeCap.BUTT
  val red        = Brush("red").color(0xFFee0000).strokeWidth(25.0f).cap(SQUARE)
  val redLine    = Brush("red").color(0xFFee0000).strokeWidth(1.0f)
  val redFrame   = Brush("red").color(0xFFee0000).strokeWidth(2.0f)
  val blue       = Brush("blue").color(0xFF0000ff).strokeWidth(2.0f)
  val blueLine   = Brush("blue").color(0xFF0000ff).strokeWidth(2.0f)
  val green      = Brush("green").color(0xFF00ff00).strokeWidth(5.0f).strokeCap(ROUND)
  val greenLine  = Brush("green").color(0xFF00ff00).strokeWidth(1.0f)
  val greenFrame = Brush("green").color(0xFF00ff00).strokeWidth(3.0f)
  val realYellow = Brush("yellow")    col 0xFFffdd00 width 75 cap ROUND antiAlias true
  val white      = Brush("white")     col 0xFFffffff
  val whiteFrame = Brush("white")     col 0xFFffffff width 5
  val black      = Brush("black")     col 0xFF000000 width 1
  val nothing    = Brush("nothing")   col 0
  val invisible  = Brush("invisible") col 0 width 1
  val lightGrey  = Brush("lightGrey") col 0xFFbbbbbb width 1
  val darkGrey   = Brush("darkGrey")  col 0xFF777777 width 1
  /** Using the new Brush API */
  val yellow     = Brush("yellow")(color=0xFFffdd00, width=75f, cap=ROUND, antiAlias = true)
}

/**
 * Concrete definitions of a variety of brushes.
 */
object DefaultBrushes extends Brushes {}

/**
 * Default brushes for use in non-styled (basic) components. These can safely be changed
 * dynamically as an application that uses non-styled components starts.
 *
 * There's a case for making these values the features of a class; and having a global
 * mapping that selects (by name) which particular instance of the class is being used
 * as the source of defaults.
 *
 * TODO: this feature is in flux.
 */
object Brushes {
  import GlyphTypes.Paint
  import io.github.humbleui.skija._


  var upFrame: Brush = Brush("Brushes.upFrame")       color 0xFF7A7A7A strokeWidth 2f strokeCap PaintStrokeCap.ROUND
  var downFrame: Brush = Brush("Brushes.downFrame")   color 0xFF777777 strokeWidth 2f strokeCap PaintStrokeCap.ROUND
  var hoverFrame: Brush = Brush("Brushes.hoverFrame") color 0xFF000000 strokeWidth 2f strokeCap PaintStrokeCap.ROUND

  var buttonFace: Typeface = FontMgr.getDefault().matchFamilyStyle("Menlo", FontStyle.NORMAL)
  var buttonPointSize: Float = 22.0f
  def buttonFont: Font = new Font(buttonFace, buttonPointSize)
  def buttonText(s: String): Text = Text(s, buttonFont)
  var buttonForeground: Brush = Brush("Brushes.buttonForeground") color 0xFF000000 strokeWidth 1.0f
  var buttonBackground: Brush = Brush("Brushes.buttonBackground") color 0xFFFFFFFF strokeWidth 1.0f
  var buttonDown:       Brush = Brush("Brushes.buttonDown") color 0xFFFF0000 strokeWidth 1.0f
  var buttonHover:      Brush = Brush("Brushes.buttonHover") color 0xFF00FF00 strokeWidth 1.0f

  var default: Brush = Brush("Brushes.default") color 0xAA000000 strokeWidth 3.0f strokeCap PaintStrokeCap. SQUARE

  /** Default paint for a point: black */
  val point: Paint = Brush("Brushes.point") color 0xFF000000 strokeWidth 1.0f
}
