package org.sufrin.glyph
package tests.demonstrationBook

import styled.{Book, BookSheet, TextButton, TextToggle}

import io.github.humbleui.skija.PaintMode
import org.sufrin.glyph.GlyphShape.{circle, lineBetween, rect, PathShape}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.NaturalSize.{Col, Row}
import org.sufrin.glyph.unstyled.{reactive, static}
import org.sufrin.glyph.unstyled.static.{FilledOval, FilledRect, Label, Rect}

import java.lang.Math.{cos, sin}

class Animation(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.pageSheet
  import pageSheet.{em, ex}
  import translation._
  val book = Book()
  val Page = book.Page
  import Brushes._

  val PseudoMechanism = Page("Pseudomechanism", "") {
    import unstyled.dynamic.{Periodic, ActiveGlyph}


    // Ratio of the size if the mobile to the stage
    val ratio: Scalar = 1.25f

    // An arbitrarily-shaped and intricate mobile
    val mobile: GlyphShape = {
      import GlyphShape._
      val shape =
        (circle(25)(red)---
          rect(150,450)(blueFrame(mode=PaintMode.STROKE))---
          circle(25)(red)) ~~~ ((pie(95)(red,blue,transparent,green,yellow)~~~circle(10)(white)) ||| rect(100, 50)(yellow))
      shape
    }



    // other elements
    import GlyphShape._
    val spot = GlyphShape.rect(12,12)(red)
    val blob = GlyphShape.rect(12,12)(white)
    val spotdelta = 0f
    val diam      = mobile.w max mobile.h
    val track     = arc(diam, diam, 0, 90.0f, true)(green(width=4, mode=STROKE))
    val track2    = arc(300, 300, 90f, 90.0f, true)(red(width=12, mode=STROKE))
    val track3    = arc(300, 300, 180f, 90.0f, false)(blue(width=12, mode=STROKE))

    // The scenery
    val scenery: GlyphShape = {
      import GlyphShape._
      track ~~~ track.turn(180) ~~~ rect(ratio*diam, ratio*diam)(lightGrey) ~~~ track2 ~~~ track2.turn(180) ~~~ track3 ~~~ track3.turn(180)
    }

    val blueString  = blueLine.sliced(5f, 3f)
    val whiteString = white(width=2).sliced(6f, 4f)
    val redString   = red(width=4).sliced(5f, 4f)

    // An active glyph whose initial image is formed by the mobile and ancillary actors
    // Its state is the current angle of rotation of the mobile
    class Stage extends ActiveGlyph[Double] (0.0){
      val orbit = scenery.w / 3f

      private val D2R: Double = Math.PI/180.0
      private val oneDegree: Double = 1.0

      // each step turns the mobile by a degree
      override def step(): Unit = set((current + oneDegree))

      private def lineConnecting(v1: TargetShape, v2: TargetShape)(fg: Brush): TargetShape  =  lineBetween(v1, v2)(fg).targetLocatedAt(0,0)

      /**
       * Vertices that will be placed whenever a frame is generated (see `toGlyph`) and
       * superimposed dynamically by "elastic" strings.
       *
       */
      object Linked {
        val blob = circle(6)(red)
        val vertex1, vertex2, vertex3: TargetShape = blob.targetLocatedAt(0, 0)
        val elasticStrings =
          superimposed(
            vertex1, lineConnecting(vertex1, vertex2)(blueString),
            vertex2, lineConnecting(vertex2, vertex3)(whiteString),
            vertex3, lineConnecting(vertex3, vertex1)(redString))
      }

      /**
       * Generate the glyph showing the next frame, by
       * placing the linked vertices then forming a glyph superimposing
       * the scenery, the turned mobile, and the placed vertices.
       */
      def toGlyph(degrees: Double): Glyph = {
        import Linked._
        locally // Place the linked vertices
        { val θ   = (degrees % 360.0) * D2R
          val dx  = orbit * Math.cos(3f * θ).toFloat
          val ddy = orbit * Math.cos(6f * θ).toFloat
          val dy  = orbit * Math.sin(θ).toFloat

          vertex1.placeAt(-orbit, ddy)
          vertex2.placeAt(dx, orbit)
          vertex3.placeAt(orbit, dy)
        }


        asGlyph(
          superimposed(
            scenery,
            mobile.turn(degrees.toFloat),
            elasticStrings
          )
        )
      }

      /** A copy of this glyph; perhaps with different foreground/background */
      def copy(fg: Brush, bg: Brush): Glyph = null
    }


    object Animation  {
      lazy val animated: Stage = new Stage
      lazy val driver:   Periodic[Double] = Periodic[Double](animated, 15.0)
    }

    val FPS = styled.ActiveString(f"${1000.0/Animation.driver.msPerFrame}%3.2f FPS")
    def setFPS(): Unit = FPS.set(f"${1000.0/Animation.driver.msPerFrame}%3.2f FPS")

    Col(align=Center)(
      <div width="60em" align="justify">
        <p>
          This is an animation test case that uses <b>GlyphShape</b>s
          to form the image(s) being animated as well as the stage on which the shapes -- which
          are not intended to mean anything -- are
          shown.
        </p>
        <p>
          Each frame of the animation is computed from scratch as it is shown;
          and the coherence of the resulting animation seems satisfactory -- even
          when the frame rate is quite high.
        </p>
      </div>, ex,
      FPS, ex,
      Row(
        TextToggle(whenFalse="Start", whenTrue="Stop", initially = false){
          case true  =>
            Animation.driver.start()
            setFPS()
          case false =>
            Animation.driver.stop()
        }, em,
        TextButton("Faster"){
          _ =>
            if (Animation.driver.msPerFrame>2) Animation.driver.msPerFrame /= 2
            setFPS()
        }, em,
        TextButton("Slower"){
          _ =>
            Animation.driver.msPerFrame *= 2
            setFPS()
        }
      ), ex, ex,
      Animation.animated, ex, ex
    )
  }

  val Rhodonea = Page("Rhodonea", "") {
    import unstyled.dynamic.{Periodic, ActiveGlyph}


    val blueString  = blueLine.sliced(5f, 3f)
    val whiteString = white(width=2).sliced(6f, 4f)
    val redString   = red(width=4).sliced(5f, 4f)

    /*
     * An active glyph whose initial image is formed by the background.
     * Its state is the current angle of rotation
     * the first `toGlyph` call will be with `degrees=0.0`
     */

    class Stage extends ActiveGlyph[Double] (0.0, Rect(600,600, blueLine, white)){

      private val D2R: Double = Math.PI/180.0
      private val oneDegree: Double = 1.0
      locally { current = -oneDegree }


      lazy val stage: Glyph    = background
      val path                 = new PathShape(red(mode=PaintMode.STROKE), absolute=true)
      val orbit                = stage.w*0.5f-10f
      var startPos             = cartesian(orbit, 0.0)
      val nib: LocatedShape    = circle(4)(blue).targetLocatedAt(0,0) // (stage.w*0.5f, stage.h*0.5f)
      val centre: LocatedShape = circle(5)(blue).targetLocatedAt(0,0)
      val line                 = lineBetween(centre, nib)(blue)
      var n, d: Double         = 1
      var stopped: Boolean     = false

      // each step turns the mobile by a degree
      override def step(): Unit = {
        var next = current + oneDegree
        set((next))
      }

      @inline private def cartesian(R: Double, θ: Double):(Scalar, Scalar) = ((R * cos(θ)).toFloat, (R * sin(θ)).toFloat)
      @inline private def closeTo(l: Scalar, r: Scalar): Boolean = (l-r).abs <= 1e-5f
      @inline private def closeTo(pos1: (Scalar,Scalar), pos2: (Scalar,Scalar)): Boolean =
              closeTo(pos1._1, pos2._1) && closeTo(pos1._2, pos2._2)

      @inline def stopAtStart(pos: (Scalar, Scalar)): Unit =
        if (closeTo(startPos, pos)) {
          stopped = true
          Animation.driver.stop()
          PressStopButton()
      }

      /**
       * Generate the glyph showing the next frame, by
       * placing the linked vertices then forming a glyph superimposing
       * the scenery, the turned mobile, and the placed vertices.
       */
      def toGlyph(degrees: Double): Glyph = {
        locally
        { import Math.{sin,cos}
          val θ = degrees * D2R
          val pos = cartesian(cos((n/d) * θ) * orbit, θ)
          if (degrees<1) path.moveTo(pos) else path.lineTo(pos)
          if (degrees%180 ==0) {
            path.addCircle(pos, 5)
            stopAtStart(pos)
          }
          nib.x = pos._1 - 2.5f
          nib.y = pos._2 - 2.5f
        }
        stage
      }


      def reset(): Unit = {
        path.reset()
        set(0.0)
      }

      override def draw(surface: Surface): Unit = {
        drawBackground(surface)
        //surface.withClip(diagonal) {
        surface.withOrigin(currentGlyph.location) {
          currentGlyph.draw(surface)
          surface.withOrigin(currentGlyph.location+(currentGlyph.diagonal * 0.5f)) {
            path.draw(surface)
            if (!stopped) {
              nib.draw(surface)
              line.draw(surface)
            }
          }
        }
        //}
      }

      /** A copy of this glyph; perhaps with different foreground/background */
      def copy(fg: Brush, bg: Brush): Glyph = null
    }


    object Animation  {
      lazy val stage:  Stage = new Stage
      lazy val driver: Periodic[Double] = Periodic[Double](stage, 250.0)

      def start(): Unit = {
        stage.reset()
        driver.start()
        PressStartButton()
      }

      def N(i: Int): Unit = { stage.n=i.toDouble; start() }
      def D(i: Int): Unit = { stage.d=i.toDouble; start() }
    }

    def PressStartButton(): Unit = startButton.set(true)
    def PressStopButton(): Unit = startButton.set(false)


    lazy val FPS = styled.ActiveString(f"Drawing speed ${1000.0/Animation.driver.msPerFrame}%3.2f °/sec")
    def setFPS(): Unit = FPS.set(f"Drawing speed ${1000.0/Animation.driver.msPerFrame}%3.2f °/sec")

    lazy val startButton =  TextToggle(whenFalse="Start", whenTrue="Stop", initially = false){
      case true  =>
        Animation.start()
        setFPS()
      case false =>
        Animation.driver.stop()
    }


    val Ns = styled.RadioCheckBoxes("1/2/3/4/5/6/7/8/9/10/11/12/13".split('/').toSeq){
      case None     => Animation.N(1)
      case Some(i)  => Animation.N(i+1)
    }

    val Ds = styled.RadioCheckBoxes("1/2/3/4/5/6/7/8/9/10/11/12/13".split('/').toSeq){
      case None     => Animation.D(1)
      case Some(i)  => Animation.D(i+1)
    }

    Col(align=Center)(
      <div width="60em" align="center">
        <p >
          Drawing Rose (rhodonea) curves of the form:
        </p>
        <p><i>r(θ) = a . cos(n/d . θ)</i></p>
        <p>
          Draw by choosing N and/or D; or use <b>Start/Stop</b>
        </p>
      </div>, ex,
      FPS, ex,
      Row(
        TextButton("Faster"){
          _ =>
            if (Animation.driver.msPerFrame>1.0) Animation.driver.msPerFrame /= 2
            setFPS()
        }, em,
        TextButton("Slower"){
          _ =>
            Animation.driver.msPerFrame *= 2
            setFPS()
        }
      ),

      ex, ex,
      Animation.stage, ex, ex,
      Label("N ") beside Ns.arrangedHorizontally() beside Label(" D ") beside Ds.arrangedHorizontally(), ex,
      startButton
    )
  }

  val SpinningButtons = Page("Spinning buttons", "") {
    import unstyled.dynamic.{Periodic, Transform, Transformable}
    val shape = static.Concentric(rowAlign=Mid, colAlign=Center)(
      FilledOval(40, 40, fg=blue),
      FilledRect(30, 10, fg=red) beside FilledRect(10, 10, fg=green))

    var lastDriver: List[Periodic[Int]] = Nil

    /**
     *
     *  A button that appears at distinct rotations and scales in successive frames.
     *  Appearances are precomputed before the animation starts. Each button is "driven"
     *  by its own lightweight thread.
     */
    class AnimatedButton() {
      lazy val button = reactive.ColourButton(shape, green, red, background = true, NoHint) {
        _ =>
          if (driver.running) driver.stop() else driver.start()
          lastDriver = List(driver)
      }

      val transforms: Seq[Transform] = {
        val steps = (5 to 10).map{ i => i.toFloat / 5 }
        val sizes = steps ++ steps.reverse
        for {scale <- sizes; rotation <- 0 to 15}
          yield { glyph: Glyph => glyph.scaled(scale).turned(rotation*22.5f, tight = true) }
      }

      /** This glyph's appearance as a sequence of  transforms applied to `button` */
      lazy val animated: Transformable = Transformable(button, transforms)
      /** The thread that selects successive transformed appearances at each frame. */
      lazy val driver:   Periodic[Int] = Periodic[Int](animated, 2.0)
    }

    val animations: Seq[AnimatedButton] = for { i<-1 to 12 } yield new AnimatedButton()

    Col(align=Center)(
      <div width="60em" align="justify">
        <p>
          A grid of rotating buttons. Individual buttons are started/stopped
          by clicking on them; and can be started or stopped together with
          the Start all / Stop all toggle button. The speed of the last
          started/stopped button(s) can be adjusted with the Faster/Slower
          buttons.
        </p>
        <p>
          Each button is animated by its own lightweight <b>Periodic</b> driver.
          Its appearance in each frame is computed in advance of the anim_ation, so
          that potentially in_efficient "online" calc_ulations are not necessary.
        </p>
      </div>, ex,
      Row(
        TextToggle(whenFalse="Start all", whenTrue="Stop all", initially = false){
          case true  =>
            lastDriver = animations.map(_.driver).toList
            for { animation <- animations } animation.driver.start()
          case false =>
            lastDriver = animations.map(_.driver).toList
            for { animation <- animations } animation.driver.stop()
        }, em,
        TextButton("Faster"){
          _ => for { driver <- lastDriver  } if (driver.msPerFrame>4) driver.msPerFrame /= 2
        }, em,
        TextButton("Slower"){
          _ => for { driver <- lastDriver  } driver.msPerFrame *= 2
        }
      ), ex, ex,
      NaturalSize.Grid(bg=lightGrey).grid(width=4)(animations.map(_.animated)), ex, ex, ex,
    )
  }

  val GUI: Glyph = book.Layout.leftCheckBoxes(buttonAlign = Right, pageAlign = Center).enlarged(30)

}
