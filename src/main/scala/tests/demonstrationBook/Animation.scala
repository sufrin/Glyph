package org.sufrin.glyph
package tests.demonstrationBook

import styled.{ActiveString, Book, BookSheet, MenuButton, TextButton, TextToggle}

import io.github.humbleui.skija.{PaintMode, PaintStrokeCap}
import org.sufrin.glyph.GlyphShape.{asGlyph, circle, composite, line, lineBetween, oval, rect, superimposed, AnimatedShape, AnimatedShapeGlyph, FILL, PathShape, STROKE}
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.NaturalSize.{Col, Grid, Row}
import org.sufrin.glyph.unstyled.{reactive, static}
import org.sufrin.glyph.unstyled.dynamic.Animateable
import org.sufrin.glyph.unstyled.static.{FilledOval, FilledRect, Label, Rect}

import java.lang.Math.{cos, log, log1p, sin}
import scala.math.{ceil, floor}

class Animation(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  implicit val pageSheet: StyleSheet = style.pageSheet
  import pageSheet.{em, ex}
  import translation._
  val book = Book()
  val Page = book.Page
  import Brushes._

  val Mechanism = Page("Mechanism", "") {
    import unstyled.dynamic.{Periodic, ActiveGlyph}

    trait SpokedWheel extends GlyphShape {
      val radius: Scalar
      val spokes: Int
      val brush, rimBrush:  Brush
      val rimWidth = 25f
      val spokeWidth = 15f
      val spokeAngle = 360f/spokes.toFloat
      val shape = {
        circle(radius+rimWidth*0.6f)(rimBrush(width=rimWidth*0.3f, mode=STROKE)) ~~~
        circle(radius)(brush(width=rimWidth, mode=STROKE))    ~~~    // rim
        circle(radius*.25f)(brush(width=rimWidth, mode=FILL)) ~~~    // hub
        superimposed(for { spoke <- 0 until spokes } yield rect(2*radius, 5)(brush(width=spokeWidth)).turn(spoke*spokeAngle, true)) ~~~
        rect(5, 2*radius)(red(width=spokeWidth/2))
      }

      def draw(surface: Surface): Unit = {
        shape.draw(surface)
      }
      def diagonal: Vec = shape.diagonal
    }

    class RotatingWheel (val radius: Scalar, val spokes: Int) extends SpokedWheel {
      lazy val brush = blue
      lazy val rimBrush = brown
      def withForeground(brush: Brush): GlyphShape = null

      def jointLocation(degrees: Scalar): Vec = {
        import Math.{cos, sin, PI}
        val theta = -degrees * (PI / 180)
        val rad = radius * 0.5
        val dx = rad*sin(theta)
        val dy = rad*cos(theta)
        (Vec(dx, dy))
      }

      def toShape(t: Double): GlyphShape = this.shape.turn(t.toFloat, tight=true)

      def withBrushes(fg: org.sufrin.glyph.Brush, bg: org.sufrin.glyph.Brush): GlyphShape = this

    }



    class Mechanism extends AnimatedShapeGlyph[Double] (0.0, Vec(900, 900)){

      override def copy(fg: Brush, bg: Brush): AnimatedShapeGlyph[Double] = new Mechanism

      private val D2R: Double = Math.PI/180.0
      private val oneDegree: Double = 1.0

      // each step turns the mobile by a degree
      override def step(): Unit = set((current - oneDegree))

      private def rodConnecting(v1: LocatedShape, v2: LocatedShape)(fg: Brush): LocatedShape  =  {
        lineBetween(v1, v2)(fg).floating()
      }

      private def rodBetween(v1: Vec, v2: Vec)(brush: Brush): GlyphShape = new GlyphShape {
        override val fg: Brush = brush
        def draw(surface: Surface): Unit = {
          val p = new PathShape(fg, false)
          p.moveTo(v1.x, v1.y)
          p.lineTo(v2.x, v2.y)
          p.draw(surface)
        }

        def diagonal: Vec = Vec.Zero

        def withForeground(brush: Brush): GlyphShape = null

        def withBrushes(fg: Brush, bg: Brush): GlyphShape = throw new java.lang.UnsupportedOperationException()
      }

      lazy val wheel1 = new RotatingWheel(100, 4)
      lazy val wheel2 = new RotatingWheel(100, 8)
      lazy val cylinder = (rect(150, 60)(green)~~~rect(150, 60)(brown(width=8, mode=STROKE, cap=PaintStrokeCap.SQUARE)))


      /**
       * Links that will be placed whenever a frame is generated (see `toGlyph`) and
       * superimposed dynamically by "elastic" strings.
       *
       */
      object Linkage {
        val joint = composite(circle(16)(red), circle(12)(black))
        lazy val joint1, joint2, joint3: LocatedShape = joint.floating()
        lazy val rod1 = rodConnecting(joint1, joint2)(brown(width=14, cap=ROUND))
        lazy val rod2 = rodConnecting(joint1, joint3)(brown(width=14, cap=ROUND))
      }

      /**
       * Generate the glyph showing the next frame, by
       * placing the roded vertices then forming a glyph superimposing
       * the scenery, the turned mobile, and the placed vertices.
       */
      def toShape(degrees: Double): GlyphShape = {
        import Linkage._
        val deg = degrees.toFloat
        val w1 = wheel1.toShape(deg).centredAt(w*0.25f, wheel1.h*0.5f+30)
        val w2 = wheel2.toShape(deg).centredAt(w*0.25f, h-wheel2.h*0.5f-30)
        joint1.centerAt(w1.center+wheel1.jointLocation(deg))
        joint2.centerAt(w2.center+wheel2.jointLocation(deg))
        joint3.centerAt(w1.center+(w1.w*2, 0))
        val cyl = cylinder.turn((joint1.center).directionTo(joint3.center)).centredAt(joint3.center)
        composite (
          w2, w1,
          joint1, joint2,
          rod1, rod2,
          cyl, joint3,
        )
      }

    }

    object Animation  {
      lazy val mechanism: Mechanism = new Mechanism
      lazy val driver:    Periodic[Double] = Periodic[Double](mechanism, 120.0)
    }

    val FPS = styled.ActiveString(f"${1000.0/Animation.driver.msPerFrame}%3.2f FPS")
    def setFPS(): Unit = FPS.set(f"${1000.0/Animation.driver.msPerFrame}%3.2f FPS")

    Col(align=Center)(
      <div width="60em" align="justify">
        <p>
          This is an animation test case that uses <b>GlyphShape</b>s
          to form the image(s) being animated as well as the stage on which the shapes are
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
      Animation.mechanism.framed(), ex, ex,
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
      val centrum: LocatedShape = circle(5)(blue).targetLocatedAt(0,0)
      val line                 = lineBetween(centrum, nib)(blue)
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

  val Spiral = Page("Spiral", "") {
    import unstyled.dynamic.{Periodic, ActiveGlyph}


    /*
     * An active glyph whose initial image is formed by the background.
     * Its state is the current angle of rotation
     * the first `toGlyph` call will be with `degrees=0.0`
     */

    class Stage extends ActiveGlyph[Double] (0.0, Rect(700,700, lightGrey(width=3).sliced(3, 2), transparent)) {

      private val D2R: Double = Math.PI/180.0
      private val oneDegree: Double = 1.0
      locally { current = -oneDegree }

      lazy val stage: Glyph    = background
      val path                 = new PathShape(red(width=0, cap=ROUND, mode=PaintMode.STROKE), absolute=true)
      val orbit                = stage.w*0.5f-30f
      var d: Double            = 0.03
      var turns: Int           = 0
      var scale: Scalar        = 1.0f

      val SHOWTURNS = ActiveString(f"Scale is $scale%1.5f after $turns%03d turns.")

      // each step turns the mobile by a degree
      override def step(): Unit = {
        var next = current + oneDegree
        set((next))
      }

      @inline private def cartesian(R: Double, θ: Double):(Scalar, Scalar) = ((R * cos(θ)).toFloat, (R * sin(θ)).toFloat)

      def F(R: Double, θ: Double): Double = R*Math.exp(d * θ)

      /**
       * Generate the glyph showing the next frame.
       */
      def toGlyph(degrees: Double): Glyph = {
          val θ = degrees * D2R
          val R = F(orbit, θ)
          val pos = cartesian(R, θ)
          if (degrees<1)
            path.moveTo(pos)
          else
            path.lineTo(pos)



        if (degrees%(360) == 0) {
          turns+= 1
          scale = {
            val f = log1p(floor(degrees / 360.0)).toFloat
            0.5f*(if (d>0) (1/f) else f)
          }
          SHOWTURNS.set(f"Scale is $scale%1.5f after ${turns}%03d turns.")
          if (turns>=125) { Animation.stop() }
        }
        stage
      }


      def reset(): Unit = {
        path.reset()
        scale = 1.0f
        turns = 0
        set(0.0)
      }

      override def draw(surface: Surface): Unit = {
        drawBackground(surface)
        surface.withClip(diagonal) {
          surface.withOrigin(currentGlyph.location) {
            currentGlyph.draw(surface)
            surface.withOrigin(currentGlyph.location + (currentGlyph.diagonal * 0.5f)) {
              surface.withScale(scale) {
                path.draw(surface)
              }
            }
          }
        }
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

      def stop(): Unit = {
        driver.stop()
        PressStopButton()
      }

      def D(i: Double): Unit = { stage.d=i.toDouble; start() }
      def D: Double = stage.d
      val SHOWTURNS = stage.SHOWTURNS
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

    object Menus {

      def chooserMenu(fieldname: String, choices: Seq[String])(choose: String => Unit): Glyph = {
        val buttons = choices.map {
          name => MenuButton(name) { _ => choose(name) }
        }
        val menu = styled.overlaydialogues.Dialogue.Menu(fieldname, nested = false, buttons)
        menu
      }

      def chooserMenu(fieldname: String, choice: String, choices: String*)(choose: String => Unit)(implicit sheet: StyleSheet): Glyph =
        chooserMenu(fieldname, choice :: choices.toList)(choose)


      def scalarMenu(fieldname: String, choices: Scalar*)(choose: Scalar => Unit)(implicit sheet: StyleSheet): Glyph =
        chooserMenu(fieldname, choices.map(_.toString)) { s => choose(s.toFloat) }

      def intMenu(fieldname: String, choices: Int*)(choose: Int => Unit)(implicit sheet: StyleSheet): Glyph =
        chooserMenu(fieldname, choices.map(_.toString)) { s => choose(s.toInt) }

    }

    val SHOWD = styled.ActiveString(f"D is ${Animation.D}%2.3f  ")

    val SHOWT = Animation.SHOWTURNS

    def setD(d: Scalar): Unit = {
      Animation.D(d)
      SHOWD.set(f"D is ${d}%2.3f")
    }

    val DFIELDL = Menus.scalarMenu("D(<0)", -0.4f, -0.3f , -0.2f, -0.1f, -0.09f, -0.07f, -0.05f, -0.03f, -0.01f, -0.009f)(setD)

    val DFIELDR = Menus.scalarMenu("D(>0)", 0.4f, 0.3f , 0.2f, 0.1f, 0.09f, 0.07f, 0.05f, 0.03f, 0.01f, 0.009f)(setD)

    Col(align=Center)(
      <div width="60em" align="center">
        <p >
          Drawing curves of the form: <i>r(θ) = a . exp(D . θ)</i>,
        </p>
        <p>scaled by <i>log(1+⌊θ/2π⌋)/2</i> (inversely when <i>D>0</i>). </p>
        <p>
          Draw by choosing D; or use <b>Start/Stop</b>
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
      Grid(width=2, padx=10, pady=10).Table(
        DFIELDL.framed(), DFIELDR.framed(),
        ), ex,
        SHOWD, ex,  SHOWT, ex,
        startButton.framed()
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



