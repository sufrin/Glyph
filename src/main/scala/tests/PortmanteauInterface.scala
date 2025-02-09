package org.sufrin.glyph
package tests
import GlyphTypes.Window
import sheeted.{Book, BookSheet, CheckBox, Label}
import NaturalSize.{Col, Row}

class PortmanteauInterface(implicit val style: BookSheet, implicit val translation: glyphXML.Translation)  {
  val book = Book()
  val Page = book.Page
  implicit val content: Sheet = style.pageSheet
  val button: Sheet = style.buttonSheet

  def confirmCloseOn(glyph: Glyph)(window: Window): Unit = {
    import sheeted.overlaydialogues.Dialogue.OKNO
    // TODO: windowdialogues need set software scale more carefully than now if autoScale
    val prompt = Row.centered(PolygonLibrary.closeButtonGlyph scaled 5 enlarged 50,
      Label("Do you want to Exit?")(content) scaled 1.5f
    ).enlarged(50)
    OKNO(prompt,
      title = "Exit Dialogue", ok = " Exit now ", no = " Continue ")(button).InFront(glyph).andThen(close => if (close) window.close())
  }


  locally {
    translation("anchor") = { _ => Glyphs.INVISIBLE() }
  }

  Page("Welcome", "") {
    val anchor = Glyphs.INVISIBLE()
    val checkBox = CheckBox(initially=false) { state => anchor.guiRoot.autoScale=state }(content.copy(buttonFrame = Styles.Decoration.Framed(fg=DefaultBrushes.blue, bg=DefaultBrushes.nothing)))
    import translation._
    translation("anchor")   = { _ => anchor }
    translation("checkbox") = { _ => checkBox }
    <body width="40em">
      <p align="justify">
        Welcome to the Portmanteau Notebook: its source code is more
        modular than that of DemonstrationNotebook -- which evolved as
        a monolith.
      </p>
      <glyph gid="anchor"/>
      <fill/>
      <row inheritwidth="true" background="nothing">
         <fill/><span>Enable window resizing:</span><glyph gid="checkbox"/><fill/>
      </row>
    </body>
  }

  Page("New Instance", "")(new PortmanteauInstantiation().GUI)

  Page("Text", "") (new PortmanteauText().GUI)

  Page("Transforms*", "") {
    val em = style.pageSheet.em
    val ex = style.pageSheet.ex
    import Glyphs._
    import GlyphTypes._
    import DefaultBrushes.{blueLine, red, redLine, redFrame, green, nothing,black, SQUARE, BUTT, blue, brown, lightGrey}
    import sheeted.Label
    val nested = Book()
    val Page = nested.DefinePage
    Page("Turn", "Turn transforms") {

      type Scalar=Float

      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)

      val h = PolygonLibrary.closeButtonGlyph.scaled(4).h
      val w = h * 0.25f

      def rect = Rect(w, h, fg = blueLine)
      // Concentric.centered(Rect(w, w/4, red), FilledOval(w, w, blue(alpha=0.3f))).framed(blue)

      def wr(deg: Scalar): Glyph = {
        val d = rect.diagonal
        val dd: Vec = d.turned(deg, Vec(d.x / 2, d.y / 2)) + Vec(d.x / 2, d.y / 2)
        Rect(dd.x, dd.y, redLine)
      }

      val (tightBox, nontightBox) = (redLine(width=2), redLine(color=green.color, width=2))

      def L(text: String, rot: Scalar, g: Glyph): Glyph =
        sheeted.Label(f" $text%1s $rot%2.2f ").above(g.turned(rot).framed(nontightBox).enlarged(8f)).framed(fg = nothing).enlarged(8f)

      def T(text: String, rot: Scalar, g: Glyph): Glyph =
        sheeted.Label(f" $text%1s $rot%2.2f ").above(g.turned(rot, tight=true).framed(tightBox).enlarged(8f)).framed(fg = nothing).enlarged(8f)

      def B(text: String, rot: Scalar, w: Scalar, h: Scalar): Glyph =
        sheeted.Label(f" ($w%2.2f, $h%2.2f)\n($rot%2.2f)").scaled(0.8f).above(rect.turnedBoxed(w, h)(rot)).framed(tightBox).enlarged(8f)

      Col.centered(
//        Paragraphs(60 * em.w, Left)("""The .turned transform with `tight=true` always yields a square bbox whose side is the larger of the two of the present glyph.
//            |For near-rotationally-symmetric glyphs this bbox
//            |fits more closely than the one yielded by`tight=false`.
//            |
//            | Hereunder R denotes a rectangle, C denotes a circular glyph, and T denotes a triangle.
//            |Tight bounding boxes are shown in red, non-tight in green.
//            |""".stripMargin),
//        ex scaled 2,
        Row(
          L("R", 0, rect),
          L("R", 25, rect),
          L("R", 45, rect),
          L("R", 70, rect),
          L("R", 90, rect),
          L("R", 135, rect),
        ),
        Row(
          L("C", 0, circ),
          L("C", 25, circ),
          L("C", 45, circ),
          L("C", 70, circ),
          L("C", 90, circ),
          L("C", 135, circ),
        ),
        Row(
          T("C", 0, circ),
          T("C", 25, circ),
          T("C", 45, circ),
          T("C", 70, circ),
          T("C", 90, circ),
          T("C", 135, circ),
        ),
        Row(
          T("R", 0, rect),
          T("R", 25, rect),
          T("R", 45, rect),
          T("R", 70, rect),
          T("R", 90, rect),
          T("R", 135, rect),
        ),
        //ex scaled 1.5f,
        sheeted.Label("R/T turned d, for d in 0, -22.5, -45, -67.5, -90"), ex,
        Row.centered(
          {
            val (hh, ww) = (4.5f * h, h / 1.5f)
            val r = Rect(ww, hh, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            Label("tight=true\n").above(Concentric(
              (r(fg = black(width = 2.5f))).turned(0f, tight = true).framed(tightBox),
              (r(fg = redLine(width = 2.5f))).turned(-22.5f, tight = true).framed(tightBox),
              (r(fg = green(width = 2.5f))).turned(-45f, tight = true).framed(tightBox),
              (r(fg = blueLine(width = 2.5f))).turned(-67.5f, tight = true).framed(tightBox),
              (r().turned(-90f, true)).framed(tightBox),
              Point(fg = red(width = 4))
            ))
          }, em, em,
          {
            val r = PolygonLibrary.regularPolygon(3, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            Label("tight=true\n").above(Concentric(
              (r(fg = black(width = 2.5f))).turned(0f, tight = true).framed(tightBox),
              (r(fg = redLine(width = 2.5f))).turned(-22.5f, tight = true).framed(tightBox),
              (r(fg = green(width = 2.5f))).turned(-45f, tight = true).framed(tightBox),
              (r(fg = blueLine(width = 2.5f))).turned(-67.5f, tight = true).framed(tightBox),
              (r()).turned(-90f, tight = true).framed(tightBox),
              Point(fg = red(width = 4))
            ))
          }, em, em, Label("non-tight\nbboxes with\ncorresponding colours\n").above
          { val c0 = black(width = 2.5f, cap=SQUARE)
            val c1 = c0(color = red.color)
            val c2 = c0(color = green.color)
            val c3 = c0(color = blue.color)
            val c4 = c0(color = brown.color)
            val c5 = c0(color = lightGrey.color)
            val r = PolygonLibrary.regularPolygon(3, fg = Brush()(width = 2.5f, color = 0XFFff00ff))
            def  f(b: Brush, d: Scalar): Glyph = r(fg=b).turned(d).framed(b)
            Concentric(
              f(c1, -22.5f),
              f(c2, -45f),
              f(c3, -67.5f),
              f(c4, -90f),
              f(c0(width=4.5f, alpha=0.3f, cap=BUTT), 0f),
              Point(fg = red(width = 4))
            )
          }
        ) scaled 0.7f
      )
    }

    Page("Tight", "") {
      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)
      val d = circ.w * (0.35f)
      val w = d*5f
      val h = w*.25f

      def rect = Rect(w, h, fg = blueLine)
      def tr = PolygonLibrary.star7(C=50f, R=50f, fg = Brush()(width = 2.5f, color = 0XFFff00ff))

      val (r, g, b) = (red(width=1, cap=SQUARE), green(width=1, cap=SQUARE), black(width=1, cap=SQUARE))
      Col.centered(Label("Tight  (red) versus non-tight (green) bounding boxes"), ex,
        Row.centered$(
          for { tight <- List(true, false) } yield
            Col.centered$(
              for { a <- List(0f, 20f, 50f, 90f, 140f, 180f, 230f, 275f) } yield
                Row.atTop(Label(f"$a%2.1f"), em scaled 2,
                  Row.atTop$(
                    for { glyph <- List(rect, tr) } yield
                      Row.atTop(
                        Concentric(
                          glyph(b).turned(a, tight).framed(if (tight) r else g).enlarged(10f),
                          Point(if (tight) r(width=4) else g(width=4))
                        ), em scaled 2)
                  )
                )
            )
        )
      )
    }

    Page("Skew", "Skew transforms") {
      import GlyphTransforms.Skewed
      import PolygonLibrary.star7

      def circ = PolygonLibrary.closeButtonGlyph.scaled(4)

      val h = PolygonLibrary.closeButtonGlyph.scaled(4).h
      val w = h * 0.25f

      def rect = Rect(w, h, fg = blueLine)

      def L(text: String, skewX: Scalar, g: Glyph): Glyph =
        Label(f" x=$skewX%1.1f ").above(Skewed(skewX, 0f)(g).framed())


      val pics = Col.centered(
        Label(".skewed(x, 0)"), ex,
        Row(
          L("C", .0f, circ),
          L("C", .3f, circ),
          L("C", .4f, circ),
          L("C", .5f, circ),
          L("C", .6f, circ),
          L("C", .7f, circ),
        ),
        ex scaled 3,
        Row(
          L("C", .0f, rect),
          L("C", .3f, rect),
          L("C", .4f, rect),
          L("C", .5f, rect),
          L("C", .6f, rect),
          L("C", .7f, rect),
        ),
      )


      val base = rect.diagonal
      val BASE = base scaled 2f
      val skews = List(-0.95f, -0.7f, -0.5f, -0.3f, 0f, 0.3f, 0.5f, 0.7f, 0.9f)

      def Sk(sx: Scalar, sy: Scalar): Glyph = Col.centered(
        Label(f"$sx%1.1f\n$sy%1.1f"), Skewed(sx, sy)(Col.centered(star7(fg=red).scaled(.15f), Rect(BASE.x, BASE.y, fg = blueLine), Label("A"))).framed()
      )

      Col.centered(
        //Paragraphs(60, Left)("g.skewed(dx, dy) skews g rightwards as y increases, and downwards as x increases"),
        ex, ex,
        Row.centered(
          Sk(0.5f, 0f), em,
          Sk(-0.5f, 0f), em,
          Sk(0f, 0.5f), em,
          Sk(0f, 0f), em,
          Sk(0f, -0.5f), em,
          Sk(0.5f, 0.5f), em,
          Sk(-0.5f, -0.5f)
        ), ex, ex,
        pics, ex, ex,
        Label("R.skewed(x, 0).above(R.skewed(0, x))"), ex,
        Row.centered$(
          skews.map { x =>
            Col.centered(
              Label(f"x = $x%3.1f"),
              FilledRect(BASE.x, BASE.y, fg = redLine).skewed(x, 0f).framed(),
              FilledRect(BASE.x, BASE.y, fg = redLine).skewed(0f, x).framed()
            ) enlarged 18f
          }) scaled 0.75f
      )
    }

    Page("Mirror", "Mirroring and skewing\n(using intrinsic Glyph Transforms)") {
      val bigAB = Label("AB").scaled(1.5f)
      val bigCD = Label("CD").scaled(1.5f)

      def framedA = bigAB().framed()

      def rotA = bigAB().rotated(3).framed()

      def rowABCD = Row(bigAB().rotated(3), bigCD().rotated(3)).framed()

      Col.centered(
        Label("S=_.skewed(0.5,0.5); M=_.mirrored(true, true)"), ex,
        ex,
        Row.centered(
          Label("g ").above(framedA.framed()), ex, ex,
          Label("S(g) ").above(framedA.skewed(0.5f, 0.5f).framed()), em, em,
          Label("M(g) ").above(framedA.mirrored(leftRight = true,
            topBottom = true).framed()), em, em,
          Label("S(M(g)) ").above(framedA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0.5f).framed()), em, em,
          Label("M(S(M(g))) ").above(framedA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0.5f).mirrored(leftRight = true, topBottom = true).framed())
        ), ex, ex, Label("s=_.skewed(0.5, 0)"), ex,
        Row.centered(
          Label("g ").above(rotA.framed()), ex, ex,
          Label("s(g) ").above(rotA.skewed(0.5f, 0f).framed()), em, em,
          Label("M(g) ").above(rotA.mirrored(leftRight = true, topBottom = true).framed()), em, em,
          Label("s(M(g)) ").above(rotA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0f).framed()), em, em,
          Label("M(s(M(g))) ").above(rotA.mirrored(leftRight = true, topBottom = true).skewed(0.5f, 0f).mirrored(leftRight = true, topBottom = true).framed()),
        ), ex, ex, Label("m=_.mirrored(false, true)"), ex,
        Row.centered(
          Label("g ").above(rowABCD.framed()), ex, ex,
          Label("s(g) ").above(rowABCD.skewed(0.5f, 0f).framed()), em, em,
          Label("m(g) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).framed()), em, em,
          Label("s(m(g)) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).skewed(0.5f, 0f).framed()), em, em,
          Label("m(s(m(g))) ").above(rowABCD.mirrored(leftRight = false, topBottom = true).skewed(0.5f, 0f).mirrored(leftRight = false, topBottom = true).framed())
        ), ex,
        Label("Notice how the row (g) of vertical glyphs was skewed\nto the right as if from the top by m(s(m(g)))\n\nThis is the same effect as Skewed(-0.5,0)"), ex,
        rowABCD.skewed(-0.5f, 0)
      )
    }

    nested.Layout.rightButtons().enlarged(20)
  }


  /*
  Page("Transforms*", "")(new PortmanteauTransforms().Layout.leftButtons())


  Page("GlyphML", "") (new PortmanteauGlyphML().GUI)
  */

  import utils.Output.withWriteBar
  import book.Layout

  lazy val asRNotebook = withWriteBar()(Layout.rightButtons())
  lazy val asLNotebook = withWriteBar()(Layout.leftButtons())
  lazy val asVNotebook = withWriteBar()(Layout.rotatedButtons(3))
  lazy val asSNotebook = withWriteBar()(Layout.skewedButtons(0.2f, 0f, uniform = true))
  lazy val asTNotebook = withWriteBar()(Layout.topButtons())
}
