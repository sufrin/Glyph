package org.sufrin.glyph.tests
import  org.sufrin.glyph._

import Glyphs._
import GlyphTypes._

/**
 *  A completely superannuated test GUI
 *
 *  When an application will run several instances of a complete GUI, it is best to
 *  define that GUI as a trait -- instantiated once per instance.
 *  The inadvertent sharing of attributes of the GUI is thereby avoided.
 *
 *  The GUI defined below was not originally going to be instantiated more than once; and
 *  my original, naive, implemntation of the "copy" button resulted in the sharing of the hardwareScale
 *  `Variable` of the dynamically-scaled column of rotated glyphs.
 *
 */
trait MediumTestGUI extends Brushes {

  import DynamicGlyphs.{DynamicallyScaled, ViewPort}
  import GlyphTransforms.Framed
  import NaturalSize.{Col, Row}


  val border     = Brush("border").color(0xFF999999).strokeWidth(4f)

  val face: Typeface = FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
  val font36 = new Font(face, 40)
  def text(s: String): Text = Text(s, font36)
  val bigGlyph1 = (text("Á pannable, tiltable, scaleable textlayout with Åççents").asGlyph(fg=red))
  val bigGlyph2 = (text("Mousewheel over this controls pan, tilt, and hardwareScale").asGlyph(fg=red))
  val bigGlyph3 = (text("(See Help)").asGlyph(fg=red))
  val bigGlyph  = Framed()(Col.centered(bigGlyph1, bigGlyph2, bigGlyph3))

  val screenWidth = bigGlyph.w // for use in sizing the Menu bar

  val rotate = text("Rotates").asGlyph(red, yellow)

  val rotateds = DynamicallyScaled()(Row.centered(
      rotate.rotated(3)
    , rotate().rotated(2)
    , rotate().rotated(1)
    , rotate().rotated(0)
    ))

  val helpText = Framed()(Col(
    Label("Yellow->Blue:      Transform all yellows to blues"),
    Label("""Small "Rotates": Downscale the "Rotates" group"""),
    Label("Save:              Save the current appearance as a .png file"),
    Label("Copy:              Start a new independent copy of the GUI"),
    Label("(Blue triangle):   Pop up a ... popup!"),
    Label("""(Mouse wheel on the red-framed textlayout):  """),
    Label(""" (with ctrl)  pans it left and right"""),
    Label(""" (with shift) scales it"""),
    Label(""" (alone)      tilts it up and down"""),
    Label("""(Ctrl+Shift) on the red-framed textlayout: resets its pan, tilt, and hardwareScale"""),
  ))

  val em              = Label("m")
  def emSpace         = FixedSize.Space(em.w, 0.0f)
  def expandableSpace = FixedSize.Space(em.w, 1.0f)

  object Toggle extends BooleanButton {

  }

  import Location._

  val menuBar: Glyph = (FixedSize.Row(screenWidth).atTop(
    ReactiveGlyphs.ShadedButton.ofString(" Help ") {
      _ => import windowdialogues.Dialogue
        Dialogue.OK(helpText, RelativeTo(SafeCopy.button)).start()
    }, emSpace,
    Toggle.onOff(whenFalse= "Yellow->Blue ",  whenTrue="  restore Yellow ", initially=false, fg=red, bg=white){
      case true  =>  yellow col  blue.getColor strokeWidth     (realYellow.strokeWidth/2f)
      case false =>  yellow col  realYellow.color strokeWidth  (realYellow.strokeWidth)
    }, emSpace,
    Toggle.onOff(whenFalse="""Small "Rotates""", whenTrue="""Restore "Rotates""", initially=false, fg=red, bg=white){
      case false => rotateds.scale.value = 1.0f
      case true  => rotateds.scale.value = 0.5f
    }, emSpace,
    Saving.button, expandableSpace,
    SafeCopy.button,
  ))

  object Saving {
    import Location._
    val button: Glyph = ReactiveGlyphs.ShadedButton("Save") { _ =>
      import windowdialogues.Dialogue
      Saving.saved += 1
      val path = s"PNG/Saved-${Saving.saved}.png"
      External.renderBitmap(Saving.root, path)
      Dialogue.OK(Row(Label("saved as: "), Label(path)), RelativeTo(button)).start()
    }
    var saved = 0
    var root: Glyph = null
  }

  val  triangularButton: ReactiveGlyph = {
    val tr = FilledPolygon(100, 100, fg = blue)((100, 0), (0, 0), (100, 100), (100, 0))
    ReactiveGlyphs.RawButton(tr, tr().rotated(2), tr().rotated(1)) {
      _ => import windowdialogues.Dialogue
        Dialogue.OK(Col(Label("This is a popup beneath the triangular button"), Label("It was placed here by pressing that button")), RelativeTo(tr)).start()
    }
  }

  val lhs = Col.centered(
      Concentric(RRect(250, 200, false, 5f, fg=red, bg=white), Label("Roundness"))
    , triangularButton
    , Concentric(Rect(250f, 250f, yellow), Rect(300f, 400f, blue() width 4))
    , ViewPort(bigGlyph(), fg=blue, bg=white)
    , rotateds
    , Label("")
  )

  val root = Col.centered(
    menuBar,
    Row(lhs))
}

/**
 * Run a (sharing-safe) instance of the interaction. Notice that the complete "scenery"
 * is rebuilt within `copyScenes`, so there is no sharing.
 */
object SafeCopy {
  import scala.language.reflectiveCalls // Scala compiler quirk?

  val button = ReactiveGlyphs.ShadedButton.ofString("Copy") { mods: Modifiers.Bitmap =>
    import io.github.humbleui.jwm.App
    val copyGUI = new MediumTestGUI {}
    new Interaction(App.makeWindow(), GlyphTransforms.Framed(fg = copyGUI.border)(copyGUI.root)).start()
  }
}


object MediumTestGUI extends MediumTestGUI {
  locally { Saving.root = root }
}

object MediumTest extends Application {
  import MediumTestGUI.{border, root}
  val title = "Medium Test"
  val GUI = GlyphTransforms.Framed(fg=border)(root)
  override
  val defaultIconPath: Option[String] = Some("./flag.png")
}
