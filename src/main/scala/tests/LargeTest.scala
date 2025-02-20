package org.sufrin.glyph
package tests

import Glyphs._
import GlyphTypes._
import Location._
import Styles.GlyphStyle
import windowdialogues.Dialogue.OK
import PolygonLibrary._

import io.github.humbleui.jwm.EventKey
import io.github.humbleui.skija.BlendMode
import org.sufrin.glyph.GlyphTransforms.Turned

trait LargeTestGUI {

  import DynamicGlyphs.OneOf
  import GlyphTransforms.{Framed, Scaled}
  import NaturalSize.{Col, Row}
  import ReactiveGlyphs.{FramedButton, RawButton, ShadedButton}
  import DefaultBrushes._

  implicit val sheet: Sheet = Sheet()

  private lazy val atPopupAnchor = East(popupAnchor)
  val face: Typeface =
    FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
  private val medFont: Font = new Font(face, 36)
  private val smallFont: Font = new Font(face, 20)
  private val largeFont: Font = new Font(face, 40)
  private val hugeFont: Font = new Font(face, 50)
  private val hugerFont: Font = new Font(face, 75)
  private val buttonFont: Font = new Font(face, 28)
  private val exg = Text(" ", medFont).asGlyph()
  private val trup =
    FilledPolygon(100, 100, fg = blue)((100, 0), (0, 0), (100, 100), (100, 0))

  private val scene1 = {
    Col.centered(
      textColumn(fg = blue)("(1) Primitive text column layouts"),
      medex,
      medex,
      textColumn(fg = red.copy(width=0))("TextColumn(align=Center) A single line\n"),
      medex,
      textColumn(fg = blue)(
        "TextColumn(align=Center) A wide line\nabove a\ncouple more lines.\n"
      ),
      medex,
      textColumn(fg = blue, align=Left)(
        "TextColumn(align=Left) A wide line\nabove a\ncouple more lines.\n"
      ),
      medex,
      textColumn(smallFont, fg = green, align=Right)(
        "TextColumn(align=Right) A few more lines\nin a smaller font\njust to see whether TextColumn makes sense.\n"
      )
    )
  }
  private val scene2 = {
    import GlyphTransforms.{Rotated, Scaled}
    def abcd(fg: Brush) = Row(skip=15f, uniform = true)(Label("A")(fg), Label("B")(fg), Label("C")(fg))

    def g(fg: Brush) = abcd(fg = fg).framed(fg)

    def b() = RawButton(g(blue), g(redFrame), g(greenFrame)) { _ => }

    Col.centered(
      textColumn(fg = blue)("(2) Framed raw button b() transformed by Rotated, Turned, and Scaled\nIntrinsic and extrinsic transforms behave identically"),
      medex,
      medex,
      Row.centered(
        Label("b().rotated(i)"),
        medex,
        Row.centered$(for {i <- 0 until 4} yield b().rotated(i)),
        medex,
        medex,
        Label("Rotated(i)(b())"),
        medex,
        Row.centered$(for {i <- 0 until 4} yield Rotated(i)(b()))
      ),
      medex,
      medex,
      Col.centered(
        Label("b().scaled(5/i)"),
        medex,
        Row.centered$(for {i <- 1 to 5} yield b().scaled(5f / i.toFloat))
      ),
      medex,
      medex,
      Col.centered(
        Label("Scaled(5/i)(b())"),
        medex,
        Row.centered$(for {i <- 1 to 5} yield Scaled(5f / i.toFloat)(b()))
      ),
      medex, medex,
      Col.centered(
        Label("b().turned(i*23)"),
        medex,
        Row.centered$(for {i <- 1 to 9} yield b().turned(i*23f))
      )
    )
  }

  private val scene3 = {
    import GlyphTransforms.Rotated
    def abcd(fg: Brush) =
      Row.centered(Label("A ")(fg), Label("B ")(fg), Label("C")(fg))

    def g(fg: Brush) = abcd(fg = fg)

    def b() = RawButton(g(blue), g(redFrame), g(greenFrame)) { _ => }

    Col.centered(
      textColumn(fg = blue)("(3) Unframed raw button b() transformed by .rotated(i), .turned(27f * i), and .scaled(1.5f) for i<-0 until 9"),
      medex,
      medex,
      Label("b().rotated(i).scaled(1.5)"),
      medex,
      Row.centered$(for {i <- 0 until 9} yield b().rotated(i).scaled(1.5f)),
      medex,
      medex,
      Label("b().turned(27f * i)"),
      medex,
      Row.centered$(for {i <- 0 until 9} yield b().turned(27f*i)),
      medex,
      medex,
      Label("b().turned(27f * i).scaled(1.5f)"),
      medex,
      Row.centered$(for {i <- 0 until 9} yield b().turned(27f*i).scaled(1.5f)),
      medex,
      medex,
      Label("b().scaled(1.5f).turned(27f * i)"),
      medex,
      Row.centered$(for {i <- 0 until 9} yield b().scaled(1.5f).turned(27f*i)),
    )
  }

  /** An invisible glyph that will act as the anchor for popups.
   * It will be placed at the (right) end of the menu bar.
   */
  private val popupAnchor: Glyph = INVISIBLE()

  /** A small but important test of placing reactives in `OneOf`s */
  private val scene4 = Framed()(
    Col.centered(
      textColumn(fg = red)(
        "(4) Sub-interfaces can inhabit a OneOf\nShaded buttons appear to move when pressed\n[this entire app is structured as a OneOf, and gave rise to the Book() component]"
      ),
      medex,
      ShadedButton("Press me [ShadedButton 1]") { _ =>
        import sheeted.windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed shaded button 1"),
            atPopupAnchor
          )
          .start()
      },
      ShadedButton("Press me [ShadedButton 2]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed shaded button 2"),
            atPopupAnchor
          )
          .start()
      },
      ShadedButton("[ShadedButton 3]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed shaded button 3 in scene 4"),
            atPopupAnchor
          )
          .start()
      },
      ShadedButton("[ShadedButton 4]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed shaded button 4 in scene 4"),
            atPopupAnchor
          )
          .start()
      }, medex scaled 3,
        textColumn(fg = blue)("(4a) Experiments with the .rounded(radius) and .dashed(onoffspecs) brush transforms\n(various strokewidths)"), medex,
      Glyphs.FilledRect(w=150,h=140, fg=blue.rounded(30f)) beside Glyphs.FilledRect(w=150,h=140, fg=red.rounded(50f)) beside Glyphs.FilledRect(w=150,h=140, fg=green.rounded(70f))
      , medex,
      Glyphs.Polygon(w=150,h=50, fg=blue.dashed(15f, 15f).strokeWidth(15f))((0,25f), (150f,25f)).edged() beside
      Glyphs.Polygon(w=150,h=50, fg=red.dashed(15f, 15f).strokeWidth(35f))((0,25f), (150f,25f)).edged() beside
      Glyphs.Polygon(w=150,h=50, fg=green.dashed(40f, 15f).strokeWidth(40f))((0,25f), (150f,25f)).edged() beside
      Glyphs.Polygon(w=250,h=50, fg=blue.dashed(40f, 40f).strokeWidth(40f).cap(ROUND))((0,25f), (250f,25f)).edged(), medex,
      medex,
    ) enlarged 30f
  )


  private val scene5 = {

    import BooleanGlyphs._

    import Toggles._

    lazy val toggle0: OnOffButton = onOff(
      "BooleanGlyphs 0 is\nOff",
      "BooleanGlyphs 0 is\nOn",
      fg = red,
      bg = white,
      initially = true
    ) { state =>
      toggle1.invert();
      toggle2.invert();
      toggle3.invert();
      toggle4.invert()
      println(s"0=>$state ${toggle1.get}")
    }

    lazy val toggle1: OnOffButton =
      onOff(initially = false, fg = red, bg = white) { _ =>
        toggle2.invert(); toggle3.invert(); toggle4.invert();
      }

    lazy val toggle2: OnOffButton =
      onOff(initially = true, fg = red, bg = white) { _ =>
        toggle3.invert(); toggle4.invert();
      }

    lazy val toggle3: OnOffButton =
      onOff(initially = false, fg = red, bg = white) { _ =>
        toggle4.invert();
      }

    lazy val toggle4: OnOffButton = onOff(
      trdown(red)(bg = yellowHuge),
      trup(green)(bg = yellowHuge),
      initially = true,
      fg = red,
      bg = white
    ) { _ => toggle0.invert() }

    def t(name: String)(toggle: OnOffButton): Glyph =
      Row.centered(textColumn()(name), toggle.scaled(0.75f))

    val allOn = ReactiveGlyphs.FramedButton("All on", fg = green) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.set(true)
      }
    }

    val allOff = ReactiveGlyphs.FramedButton("All off", fg = red) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.set(false)
      }
    }

    val allFlip = ReactiveGlyphs.FramedButton("Flip all", fg = blue) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.invert()
      }
    }

    def fill = FixedSize.Space(0f, 1f)

    val caption = textColumn()(
      "(5) Five dependent toggles with states shown in various ways.\nFlipping each causes others to change state."
    )

      Col.centered(
        caption,
        medex,
        FixedSize
          .Row(caption.w)
          .centered(
            fill,
            toggle0,
            fill,
            t("1")(toggle1),
            fill,
            t("2")(toggle2),
            fill,
            t("3")(toggle3),
            fill,
            t("4")(toggle4),
            fill
          ),
        medex,
        Row.centered(allOff, allFlip, allOn)
      ).enlarged(30f).edged(redFrame)
  }

  import Location._

  private val scene6 = {
    val anchor1 = Label("")

    val theText = TextField(
      blue,
      white,
      buttonFont,
      size = 40,
      onEnter = { s =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            Label(s"You typed $s"),
            RelativeTo(anchor1),
            "Report from TextField"
          )
          .start()
      }
    )

    Col.centered(
      Label("(6) TextField: a textlayout-entry field"),
      medex,
      medex,
      medex,
      Label("(the cursor is always kept in view)"),
      Label("(visual cues are given for off-field textlayout)"),
      Label(" "),
      Label("Ctrl/Cmd C - copy all"),
      Label("Ctrl/Cmd X - cut all to clipboard"),
      Label("Ctrl/Cmd V - insert from clipboard"),
      Label("Home/End/Left/Right/Backspace"),
      Label(" "),
      Framed(blue)(theText),
      Label(""),
      ShadedButton(
        "OS/X Symbols palette (double-click on a symbol to insert it)"
      ) { mods: Modifiers.Bitmap =>
        import Modifiers.{Middle, Secondary}
        if (mods.include(Secondary | Middle))
          OK(
            Col.centered(
              Label("Symbols palette not available (simulated error)")
            ),
            RelativeTo(anchor1),
            "Unimplemented"
          ).start()
        else
          try io.github.humbleui.jwm.App.openSymbolsPalette()
          catch {
            case _: Throwable =>
              OK(
                Col.centered(Label("Symbols palette not available")),
                RelativeTo(anchor1),
                "Unimplemented"
              ).start()
          }
      },
      anchor1
    )
  }
  private val scene7 = {
    val huge = Text("Huge∑y", hugeFont)
    val large = Text("Large∑y", largeFont)
    val med = Text("Medium∑y", medFont)
    val small = Text("Small(y)", smallFont)
    val giant = Text("Giant∑y", hugerFont)
    val sp = Text("", medFont)
    def blob = Glyphs.FilledRect(5f,med.h,fg=red)
    def tab=FixedSize.Space(1.0f,1.0f)

    def space = sp.asGlyph()

    val texts = List(tab, huge, sp, large, sp, med, sp, small, sp, huge, tab)

    def atTop       = NaturalSize.Row(align=Top)(texts.map(_.copy(fg=red)))
    def atMid       = NaturalSize.Row(align=Mid)(texts.map(_.copy(fg=green)))
    def atBottom    = NaturalSize.Row(align=Bottom)(texts.map(_.copy(fg=blue)))
    def atBaseline  = NaturalSize.Row(align=Baseline)(texts.map(_.copy(fg=black))).debugGeometry

    Col.centered(
      textColumn()(
        """(7) Various NaturalSize.Row(align=...) alignments
          |showing baselines explicitly when present.
          |
          |Top(red), Mid(green), Bottom(blue), Baseline(black)
          """.stripMargin
      ),
      medex scaled 2, atTop.edged(),
      medex, atMid.edged(),
      medex, atBottom.edged(),
      medex, atBaseline.edged(),
      medex scaled 2,
      textColumn()("A Row(align=Baseline) of two Row(align=Baseline)\nshowing inheritance of baselines"),
      Row(Baseline)(atBaseline, Row(align=Baseline)(huge(),large(),med()).debugGeometry).edged(),
      medex, textColumn()("FixedSize.Row(align=Baseline)"),
      FixedSize.Row(width=atBaseline.w*1.3f, align=Baseline)(texts.map(_.copy())).debugGeometry.edged()
    )
  }


  private val scene8 = {
    val fatYellow: Brush = yellowHuge.copy strokeWidth 40
    val fatGreen: Brush = fatYellow.copy col 0xff00ff00
    val fatRed: Brush = fatYellow.copy col 0xffff0000
    val glyph: Glyph = Text("RawButton", medFont).asGlyph()

    val b1 = RawButton(glyph(), red, green) { _ => }
    val b2 = RawButton(glyph().enlarged(50f), red, green) { _ => }.framed()
    val b3 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }.framed()
    val b4 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }
      .enlarged(50f, bg = red)
      .framed()
    val b5 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }

    def sp = Label(" ")

    Col.centered(
      Label("(8) Raw buttons from glyph=Text(\"RawButton\", medFont).asGlyph() (try moving/pressing the mouse on these)"),
      sp,
      Col.centered(b1, Label(" RawButton(glyph,red, green)")),
      sp,
      Col.centered(
        b2,
        Label(" RawButton(glyph.enlarged(50f), red, green).framed()")
      ),
      sp,
      Col.centered(
        b3,
        Label(" RawButton(glyph.framed(fatyellow), fatred, fatgreen).framed()")
      ),
      sp,
      Col.centered(
        b4,
        Label(
          " RawButton(glyph.framed(fatyellow), fatred, fatgreen).enlarged(50f, bg=red).framed()"
        )
      ),
      sp,
      Col.centered(
        b5,
        Label(" RawButton(glyph.framed(fatyellow), fatred, fatgreen)")
      ),
      sp
    )
  }.scaled(0.9f)
  private val scene9 = {
    val sp = Text(" ", medFont)
    import GlyphTypes._

    def space = sp.asGlyph()

    val n = 0
    val button: Glyph = Text("Raw", medFont).asGlyph(blue)
    val raw = RawButton(
      button.rotated(n),
      button(red).rotated(n),
      button(green).rotated(n)
    ) { _ => }

    def but(rot: Int, scale: Scalar): Glyph = {
      Row(
        Col.centered(
          Label(s".rot($rot).scaled($scale)"),
          raw().rotated(rot).scaled(scale)
        ),
        space
      )
    }

    Col.centered(
      Label("(9) Rotated then scaled raw buttons").scaled(1.2f),
      Label("[rotation and scaling commute]"),
      space,
      Row(
        but(0, .75f),
        but(0, 1f),
        but(0, 1.3f)
      ),
      space,
      Row(
        but(2, .75f),
        but(2, 1f),
        but(2, 1.3f)
      ),
      Row(
        but(1, .75f),
        but(1, 1f),
        but(1, 1.3f)
      ),
      space,
      Row(
        but(3, .75f),
        but(3, 1f),
        but(3, 1.3f)
      ),
      space
    )
  }
  private val scene10 = {
    val sp = Text(" ", hugeFont)

    def space = sp.asGlyph()

    val caption: Text = Text("Raw", medFont)

    def button(n: Int)(fg: Brush): Glyph =
      caption.asGlyph(fg).rotated(n).framed(redFrame)

    val raw = RawButton(button(1)(blue), button(3)(red), button(3)(green)) {
      _ =>
    }

    Col.centered(
      textColumn(buttonFont, blue)(
        """(10) Scaling/rotation when making buttons.
          |[Checking reactive tracking under geometry transforms]
          |
          |Here BUT is a framed(red) RawButton with different
          |rotations/colours for its up/down/hover states""".stripMargin),
      space,
      space,
      Label(
        "BUT scaled (0.5, 0.75, 1.0, 1.2, 1.5, 2.0) + BUT.scaled(2).rotated(3)",
        fg = red
      ),
      space,
      Row.centered(
        raw().scaled(.5f),
        space,
        raw().scaled(.75f),
        space,
        raw().scaled(1f),
        space,
        raw().scaled(1.2f),
        space,
        raw().scaled(1.5f),
        space,
        raw().scaled(2f),
        Label("   +   "),
        raw().scaled(2f).rotated(3)
      )
    )
  }
  private val scene11 = {
    import GlyphTypes._

    def space = Text(" ", medFont)

    def button(scale: Scalar)(fg: Brush): Glyph = Text("Button", medFont, fg).scaled(scale)

    def rect(scale: Scalar)(fg: Brush) = FilledRect(30, 30, fg).scaled(scale)

    def rawB(scale: Scalar) =
      RawButton(button(scale)(blue), button(scale)(red), button(scale)(green)) {
        _ =>
      }.framed()

    def frameB(scale: Scalar)(fg: Brush) =
      ShadedButton(button(scale)(fg), fg=fg, bg=nothing, delta=6f){_ => }

    def rawR(scale: Scalar) =
      RawButton(rect(scale)(blue), rect(scale)(red), rect(scale)(green)) { _ =>
      }

    def rowA(raw: Scalar => Glyph) = Row.centered(
      Label("(1.5) "),
      raw(1.5f),
      space,
      Label("(2.5) "),
      raw(2.5f),
      space.scaled(1.5f)
    )

    def rowB(raw: Scalar => Glyph) = Row.centered(
      Label("(.75) "),
      raw(.75f),
      space,
      Label("(1.0) "),
      raw(1.0f),
      space
    )

    Col.centered(
      textColumn(smallFont, blue)(
        """(11) Making buttons of pre-scaled glyphs
          |""".stripMargin),
      space,
      rowA(rawB),
      rowB(rawB),
      rowA(rawR),
      rowB(rawR),
      space,
      textColumn(smallFont, red)(
        """Post-scaling buttons with intrinsic frames, such as shaded buttons,  may yield different appearances
          |because the frame is also scaled; but the behaviour is the same""".stripMargin
      ),
      space,
      Row.centered(
        Label("Scaled 2.5 before framing"),
        space,
        frameB(2.5f)(blue),
        space,
        space,
        Label("Scaled 2.5 after framing"), space,
        frameB(1f)(blue).scaled(2.5f),
      )
    )
  }

  private val scene12 = {
    import GlyphTransforms.Rotated
    val space = Text(" ", medFont).asGlyph()

    def sp: Glyph = space()

    def sliver: Glyph = sp scaled 0.3f

    val text = Text("Text", medFont).asGlyph(fg = blue)
    val rawbut: Glyph = RawButton(text, text(red), text(green)) { _ => println(s"$text pressed")}

    def rotOfBut(q: Int): Glyph =
      Col.centered(sliver, Label(s"But(Text).rotated($q).edged()"), sliver, (rawbut().rotated(q).edged()))

    def turnOfBut(degrees: Scalar): Glyph =
      Col.centered(sliver, Label(s"But(Text).turned($degrees).edged()"), sliver, (rawbut().turned(degrees)).edged())

    def butOfTurn(degrees: Scalar): Glyph =
      Col.centered(sliver, Label(s"But(Text.edged().turned($degrees))"), sliver, (rawbut().edged().turned(degrees)))

    def butOfRot(q: Int): Glyph = Col.centered(
      Label(s"But(Text.rotated($q)).edged()"), sliver,
      RawButton(text.rotated(q), text(red).rotated(q), text(green).rotated(q)) {
        _ =>
      }.edged()
    )

    Col
      .centered(
        textColumn(buttonFont, blue)(
          """(12) Testing reactive-tracking under geometry transforms.
            |
            |Buttons buttons of a pre-rotated text glyph.
            |""".stripMargin
        ),
        Row(butOfRot(0), sp, butOfRot(2), sp, butOfRot(1), sp, butOfRot(3)),
        medex,
        textColumn(buttonFont, blue)("Post-Rotated and turned buttons of the same glyph"),
        Row(
          rotOfBut(0),
          sp,
          rotOfBut(2),
          sp,
          rotOfBut(1),
          sp,
          rotOfBut(3)
        ), medex,
        Row(
          turnOfBut(20),
          sp,
          turnOfBut(40),
          sp,
          turnOfBut(60),
          sp,
          turnOfBut(80)
        ),
        medex,
        textColumn(buttonFont, blue)("Buttons of the same glyph after framing then turning"),
        Row(
          butOfTurn(20),
          sp,
          butOfTurn(40),
          sp,
          butOfTurn(60),
          sp,
          butOfTurn(80)
        ),
      )
      .enlarged(sp.w)
  }

  private val scene13: Glyph = {
    import GlyphTransforms.Enlarged
    Col.centered(
      Label("(13) Tracking reactive glyphs when there are many visible"),
      medex,
      Col.centered$(
        for {j <- 0 until 12} yield Row.centered$({
          val buttons =
            for {i <- 0 until 13} yield sceneButton(i).rotated((i + j) % 4)
          val rh = buttons.map(_.h).max
          val rw = buttons.map(_.w).max
          buttons.map(Enlarged.toSize(rw, rh)(_))
        })
      )
    )
  }
  private val scene14 = {
    def measure(t: Text, g: Glyph): Glyph = {
      val x = t.width / 2
      Envelope()(
        g @@ Vec.Origin,
        Rect(
          2,
          t.height + t.descent,
          fg = red(width = 2, cap = DefaultBrushes.SQUARE)
        ) @@ Vec(x, 0),
        Rect(
          2,
          t.height,
          fg = blue(width = 2, cap = DefaultBrushes.SQUARE)
        ) @@ Vec(x + 4, 0),
        Rect(
          2,
          t.descent,
          fg = green(width = 2, cap = DefaultBrushes.SQUARE)
        ) @@ Vec(x + 8, t.height),
        Rect(
          t.width,
          2,
          fg = black(width = 1, cap = DefaultBrushes.SQUARE, alpha = 0.25f)
        ) @@ Vec(0, t.height),
        Polygon(
          g.diagonal.x,
          g.diagonal.y,
          fg = black(width = 1, alpha = 0.25f)
        )((0f, 0f), (g.diagonal.x, g.diagonal.y)),
        Polygon(
          t.diagonal.x,
          t.diagonal.y,
          fg = green(width = 1, alpha = 0.35f)
        )((t.diagonal.x, 0f), (0f, t.diagonal.y))
      ).framed() scaled 1.5f // multiple transforms to test contains under transformation
    }

    val textA = Text("Á y", hugerFont)
    val textG = Text("g Å", hugerFont)
    val bodyA = textA.asGlyph(green)
    val bodyG = textG.asGlyph(green)

    Col.centered(
      textColumn(smallFont)(
        "(14) Glyph (in green) & RawButton (in blue) measures differ slightly.\n(RawButton.exact avoids this)"
      ),
      medex,
      Row(
        Label("RawButton"),
        measure(textG, bodyG),
        measure(
          textG,
          RawButton(bodyG(blue), bodyG(red), bodyG(yellowHuge)) { _ => }
        )
      ),
      medex,
      Row(
        Label("RawButton.exact"),
        measure(textG, bodyG),
        measure(
          textG,
          RawButton.exact(bodyG(blue), bodyG(red), bodyG(yellowHuge)) { _ => }
        )
      ),
      medex,
      Row(
        Label("RawButton.exact"),
        measure(textA, bodyA),
        medex,
        measure(textA, RawButton.exact(bodyA(blue), red, yellowHuge) { _ => })
      )
    )
  }
  private val scene15 = {
    def flag: Glyph = {
      val white = Brush("white").color(0xffffffff)
      val black = Brush("black").color(0xff000000)
      val green = Brush("green").color(0xff00aa00)
      val red = Brush("red").color(0xffee0000)

      val triangle = FilledPolygon(400, 240, red)(
        (0, 0),
        (0, 240),
        (160, 120)
      )
      Framed(black)(
        Envelope()(
          Col(
            FilledRect(450, 80, black),
            FilledRect(450, 80, white),
            FilledRect(450, 80, green)
          ),
          triangle
        )
      )
    }

    Col.centered(
      Label("(15) An image synthesised as a glyph"),
      medex,
      new Glyphs.Image(flag scaled 1.2f)
    )
  }
  val bb: Brush = Brush("blue")(color = 0xff0000ff, width = 1.0f, cap = ROUND)
  val cc: Brush = bb(width = 15f, name = "wide blue")
  private val scene16 = {

    val starSize = polyStar7(black).diagonal

    Col.centered(
      textColumn(fg = blue)(
        "(16) Brushes with path effects\n(here several are shown, including .rounded, .dashed, .blurred, and .sliced)"
      ).enlarged(35).framed(wibbly(redFrame(width = 16f))),
      medex,
      Row(
        FilledRect(starSize.x, starSize.y, red),
        medex,
        FilledRect(starSize.x, starSize.y, red.rounded(starSize.x)),
        medex,
        filledStar7(red),
        medex,
        filledStar7(wobbly(blue)),
        medex,
        polyStar7(wibbly(blue)),
        medex,
        polyStar7(redLine),
        medex,
        polyStar7(redLine.dashed(15f, 10f))
      ) scaled 0.75f, medex,
      Row(
        FilledRect(starSize.x, starSize.y, red.blurred(20f)),
        medex,
        FilledRect(starSize.x, starSize.y, red.blurred(40f)),
        medex,
        filledStar7(red.blurred(10f)),
        medex,
        filledStar7(wobbly(blue).blurred(20f)),
        medex,
        filledStar7((blueLine.blurred(30f))),
        medex,
        filledStar7((blueLine.blurred(40f))),
      ) scaled 0.75f,
      medex
    )
  }

  private val scene17 = {
    val sides = List(3f, 5f, 7f, 9f, 11f, 13f)
    val gons = sides.map { n: Float =>
      Concentric(
        Label(s"${n.toInt}", medFont),
        openStargon(n.toInt, R = 200, C = 200, fg = blue(width = 6), bg = red(alpha = 0.3f))
      )
    }
    val fgons = sides.map { n: Float =>
      Concentric(
        filledRegularPolygon(
          n.toInt,
          fg = yellowHuge(width = 9),
          bg = blue(alpha = 0.1f)
        ),
        Label(s"${n.toInt}", medFont)
      )
    }
    Col.centered(
      textColumn()("(17) Polygons and Stargons"),
      medex,
      filledStar7(wobbly(blue)),
      medex,
      Row.centered$(gons.map(_.scaled(0.5f))),
      medex,
      Row.centered$(fgons.map(_.scaled(0.5f))),
      medex,
      Row(
        Concentric.centered$(sides.map { n: Float =>
          regularPolygon(n.toInt, fg = red(width = 1, alpha = 3f / n))
        }),
        medex,
        Concentric.centered$(sides.map { n: Float =>
          regularPolygon(n.toInt, fg = blue(width = 3, alpha = 3f / n))
        })
      )
    )
  }

  private val scene18 = {
    import sheeted._
    implicit val sheet: Sheet = Sheet()
    import sheet.{ex,em}

    val starSize=filledStar7(nothing).diagonal
    val scale=starSize.y / trup.h

    Col.centered(
      Label("(18) Some homebrewed simple buttons.\nAll are edged to show their extent."),
      ex, ex,
      TextButton("This is a button that has been edged()") { state =>
        println(s"TextButton $state")
      } . edged(),
      ex, ex,
      Label("These two GlyphButtons each show different shapes"),
      Label("when up, when hovering, and when pressed\n(responding to hovering/pressing requires the cursor to be within the showing shape)."),
      ex,ex,
      GlyphButton(trup.scaled(scale), trdown.scaled(scale), trhover.scaled(scale)) { state =>
        println(s"GlyphButton $state")
      } .edged() beside ex beside
      GlyphButton(
        filledStar7(wobbly(blue)),
        filledStar7(red) rotated 2,
        filledStar7(wobbly(green)) rotated 1
      ) { state =>
        println(s"GlyphButton $state")
      } .edged(),
      ex, ex,
      Label("Three ColourButtons\nThe second changes background when the mouse is hovering/pressed"),
      ex, ex,
      Row.centered(
        ReactiveGlyphs.ColourButton(filledStar7(wobbly(blue)), red, green, background = false){ _ => } . edged(), em,
        ReactiveGlyphs.ColourButton(filledStar7(wobbly(blue)), red, green, background = true){ _ => } . edged(), em,
        ReactiveGlyphs.ColourButton("A ColourButton with a wobbly Frame", blue(width=0), red, green, background = false){ _ => }.enlarged(4).edged(wobbly(blueFrame)),
      )
    )
  }

  import PolygonLibrary._
    private val scene19 = {
      import sheeted._
      implicit val sheet: Sheet = Sheet()
      import sheet.{ex,em}

      val upColor     = yellowHuge(name = "yellow", width = 0)
      val downColor   = red(name = "red", width = 0)
      val hoverColor  = green(name = "green", width = 0)
      val noEffect    = hoverColor.pathEffect
      val wibEffect    = wibbly(green).pathEffect

       //Each of these checkboxes MUTATEs its associated colour
      val tUp = CheckBox(initially = false) {
        case true  => upColor pathEffect wibEffect
        case false => upColor pathEffect noEffect
      }
      val tDown = CheckBox(initially = false) {
        case true  => downColor pathEffect wibEffect
        case false => downColor pathEffect noEffect
      }
      val tHover = CheckBox(initially = false) {
        case true  => hoverColor pathEffect wibEffect
        case false => hoverColor pathEffect noEffect
      }

      Col.centered(
        Label(
          "(19) GlyphButtons made with polygonal glyphs\ndemonstrating contrasts in reactions to hovering/pressing"
        ),
        ex,
        Row.centered(
          Label("Use \"wobbly\" paint for up:"),
          tUp,
          Label(", down:"),
          tDown,
          Label(", hover:"),
          tHover
        ),
        ex, ex,
        Label("Glyphbutton made with polygonal glyphs, then framed\n(the cursor must enter filled part of the glyph)"),ex,
        GlyphButton(
          filledStar7(upColor),
          filledStar7(downColor)  rotated 2,
          filledStar7(hoverColor) rotated 1
        ) { _ =>
          overlaydialogues.Dialogue
            .OK(Label("You pressed the top button"))
            .InFront(tHover)
            .start()
        }.edged().scaled(.75f),
        ex,
        Label(
          "GlyphButton made with framed polygonal glyphs\n(the cursor need only enter the bounding box)"
        ),
        ex,
        GlyphButton(
          filledStar7(upColor).enlarged(20).edged(),
          filledStar7(downColor).rotated(2).enlarged(20).edged(),
          filledStar7(hoverColor).rotated(1).enlarged(20).edged(),
          exact = false
        ) { _ =>
          overlaydialogues.Dialogue
            .OK(Label("You pressed the bottom button"))
            .InFront(tHover)
            .start()
        }.scaled(.75f)
      )
    }

  private val scene20 = {
    import ReactiveGlyphs.{TextButton => But}

    val fatYellow: Brush = yellowHuge.copy strokeWidth 40
    val fatGreen: Brush = fatYellow.copy col 0xff00ff00
    val fatRed: Brush = fatYellow.copy col 0xffff0000

    val title = textColumn(smallFont, black)("(20) Reactive elements within transformed rows/columns.\nNote that framing by .edged never requires a .enlarge\nbut using .framed sometimes does")

    def Tb(title: String) = But(title) { _ => println(s"Button $title") }

    def b1 = Tb("b1")

    def b2 = Tb("b2")

    def b3 = Tb("b3").scaled(1.4f)

    def b4 = Tb("b4").scaled(1.4f)

    val buttons = List(b1, b2, b3, b4)
    val reddish = redFrame//.copy(width = 0)
    val blueish = blueLine//.copy(width = 0)
    val greenish = greenLine//.copy(width = 0)
    val triv = false

    def separator: Rect = {
      Rect(100, 20, nothing)
    }

    if (triv)
      Col()(
        Col(b1, b2, b3, b4),
        separator,
        (Col(b1, b2, b3, b4, b3, b2, b1)).rotated(1),
        separator,
        (Col(b1, b2, b3, b4)).rotated(2),
        separator,
        (Col(b1, b2, b3, b4)).rotated(3)
      )
    else
      Col().centered(
        title, separator,
        Row(frame = reddish, skip = 5f, uniform = true).atTop(
          Col(frame = blueish)(b1, b2, b3, b4),
          Col(frame = blueish)(b2, b3),
          Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).edged(reddish)
          beside Label(".edged (reddish)") beside separator beside
          Row(frame = greenish, skip = 5f, uniform = true).atTop(
            Col(frame = blueish)(b1, b2, b3, b4),
            Col(frame = blueish)(b2, b3),
            Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).enlarged(20).framed(reddish)
            beside Label(".enlarged(20).framed(reddish)"),
        separator,
        Row().centered(
          Row(frame = reddish, skip = 100f, uniform = true).atTop(
            Col(uniform = true, frame = blueish).centered(b1, b2, b3, b4),
            Col(uniform = true, frame = blueish).centered(b2, b3),
            Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).edged(reddish),
          separator,
          Row(frame = reddish).atTop(
            Col(uniform = true, frame = blueish).centered(b1, b2, b3, b4),
            Col(uniform = true, frame = blueish).centered(b2, b3).rotated(3),
            Col(uniform = true, frame = greenish)(b1, b3).framed(blueish).rotated(2),
            Col(uniform = true, frame = greenish)(b1, b3).framed(blueish).rotated(3)
          ).edged(reddish)
        ) beside separator beside
          Col(uniform = false, frame = reddish) (Row(frame = blueish).centered(b1, b2, b3, b4),
          Row(frame = blueish).centered(b2, b3),
          Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).edged(reddish)
      )
  }

  private val scene21 = {
    import ReactiveGlyphs.{TextButton => But}

    val fatYellow: Brush = yellowHuge.copy strokeWidth 40
    val fatGreen: Brush = fatYellow.copy col 0xff00ff00
    val fatRed: Brush = fatYellow.copy col 0xffff0000

    val title = textColumn(smallFont, black)(
      """(21) Reactive elements within transformed grids/rows/columns.
        |
        |
        |""".stripMargin)

    val thinBlue = DefaultBrushes.blue(width = 4)

    def Tb(title: String) = But(title) { _ => println(s"Button $title") }

    def Rb(w: Scalar, h: Scalar, brush1: Brush = fatYellow, brush2: Brush = fatGreen, brush3: Brush = fatRed) = {
      val r = FilledRect(w, h, brush1) // Row(FilledRect(w, h, brush1), FilledRect(w/20, h, brush2))
      ReactiveGlyphs.RawButton(r, r(brush2), r(brush3)) { _ => println(s"Button ($w,$h)") }.edged(thinBlue)
    }

    def Eb(w: Scalar, h: Scalar, brush1: Brush = fatYellow, brush2: Brush = fatGreen, brush3: Brush = fatRed) = {
      val r = FilledOval(w, h, brush1)
      ReactiveGlyphs.RawButton.exact(r, r(brush2), r(brush3)) { _ => println(s"Button ($w,$h)") }.edged(thinBlue, bg = thinBlue)
    }

    def b1 = Tb("b1")

    def b2 = Tb("b2")

    def b3 = Tb("b3") //.scaled(1.4f)

    def b4 = Tb("b4") //.scaled(1.6f)

    def buttons = List(b1, b2, b3, b4)

    val reddish: Brush = redFrame.copy(width = 0, blendMode = BlendMode.SRC, alpha = 0.1f)
    val blueish: Brush = blueLine.copy(width = 0)
    val greenish: Brush = green.copy(width = 0)
    val t1 = Col(bg = blueish(alpha = 0.1f))(Eb(50, 50), Rb(75, 50), Rb(100, 50))
    val t2 = (b4.enlarged(20, bg = blueish(blendMode = BlendMode.SRC, alpha = 0.3f))).turned(-30f, bg = reddish(alpha = 0.1f))

    def grid = NaturalSize.Grid(fg = black, padx = 5f, pady = 5f).table(height = 2)(buttons).enlarged(10f)

    def table = NaturalSize.Grid(fg = black, padx = 5f, pady = 5f).table(width = 2)(buttons).enlarged(10f)

    val column = Col(uniform = true).centered(b1().enlargedTo(b1.w * 2, b1.h * 2), b2().rotated(1), b3().rotated(2)).edged(blueFrame)
    val rho = Row(uniform = true).centered(b1().enlargedTo(b1.w * 2, b1.h * 2), b2().rotated(1), b3().rotated(2)).framed(blueFrame)

    def separator: Rect = {
      Rect(100, 50, nothing)
    }

    val tT = false
    Col.centered(
      title,
      separator,
      NaturalSize.Grid(padx = 50, pady = 20, fg = reddish, width = 2).rows(
        textColumn(smallFont, black)("Turned 0.1,45,67.5,85.5"), textColumn(smallFont, black)("Rotated 0,1,2,3"),
        Row()(t1().turned(0.1f, tT).framed(), t1().turned(45f, tT).framed(),
          t1().turned(67.5f, tT).framed(), t1().turned(85.5f, tT).framed()),
        Row()(t1().rotated(0).framed(), t1().rotated(1).framed(), t1.rotated(2).framed(), t1().rotated(3).framed()),
        Row()(t2().turned(0.01f, tT).framed(), t2().turned(55f, tT).framed(),
          t2().turned(67.5f, tT).framed(), t2().turned(85.5f, tT).framed()),
        Row()(t2().rotated(0).framed(), t2().rotated(1).framed(), t2().rotated(2).framed(), t2().rotated(3).framed())),
      separator scaled 0.5f,
      NaturalSize.Grid(padx = 50, fg = reddish, width = 2).rows(
        NaturalSize.Grid(padx = 20, pady = 20, height = 4)(
          grid,
          grid.rotated(1),
          grid.rotated(2),
          grid.rotated(3),
          table,
          table.rotated(1),
          table.rotated(2),
          table.rotated(3)
        ),
        NaturalSize.Grid(pady = 15, width = 1)(
          Row().centered(rho().mirrored(true, false), rho().skewed(-0.2f, 0f), rho().skewed(0.5f, 0f)),
          Row()(column().rotated(3), column().rotated(1)),
          Row(frame = reddish, uniform = true)(b1, b2, b3, b4).framed(reddish).skewed(0.2f, 0),
          Row(frame = reddish, uniform = true)(b1, b2, b3, b4).framed(reddish).skewed(0.5f, 0),
          Row(frame = reddish, uniform = true)(b1, b2, b3, b4).framed(reddish).skewed(-0.2f, 0),
          Row(frame = reddish, uniform = true)(b1, b2, b3, b4).framed(reddish).skewed(-0.5f, 0),
        ),
        (column().skewed(-0.2f, 0f) beside column().skewed(0.2f, 0f)),
        (rho().skewed(0f, 0.2f) above rho().skewed(0f, -0.2f))

      ).scaled(0.75f))
  }

  object Resizing extends BooleanButton {
    lazy val enableButton: Glyph =
      onOff(initially=false, fg=green, bg=red) {
        state =>
          enableButton.guiRoot.autoScale = state
      }.edged(fg=redFrame)
  }


  private val helpGUI = Framed(whiteFrame)(
    Col(align=Center)(
    textColumn(smallFont, black, align=Left)(
      """This is test of some basic Glyph kit components.
        |
        |If its window doesn't fit on your screen, or if you'd like it to be bigger, first click the window
        |resizing checkbox to enable dynamic resizing; then drag one of the window edges. While you are dragging,
        |the window will be continuously resized (keeping the same aspect ratio) and its content rescaled accordingly.
        |""".stripMargin
    ), medex,
    NaturalSize.Row(align=Mid, skip=10)(Label("Window resizing enabled "), Resizing.enableButton).framed(redWide.copy(cap=ROUND)),
    textColumn(smallFont, black, align=Left)("""
        |
        |The test has a few  numbered scenes that can be accessed in rotation by typing one of the "←" and "→" keys,
        |or clicking one of the buttons at the left end of the menu bar or one of the numbered buttons.
        |
        |The scenes arose arose organically during development so some now appear redundant. We have nevertheless continued to
        |maintain the application because it is a good regression test for the most basic functions of the kit.
        |
        |Elsewhere there are demonstrations of some of the derived GUI components, such as styled texts, menus, and popups.""".stripMargin
    )
  ))

  private val scene0 = {
    Col.centered(helpGUI)
  } // the preferred scene
  /** All the scenes in the test */
  private val scenes = List(
    scene0,
    scene1,
    scene2,
    scene3,
    scene4,
    scene5,
    scene6,
    scene7,
    scene8,
    scene9,
    scene10,
    scene11,
    scene12,
    scene13,
    scene14,
    scene15,
    scene16,
    scene17,
    scene18,
    scene19,
    scene20,
    scene21
  )
  /** Width of the menu bar */
  private val screenWidth = scenes.map(_.w).max
  val oneOf: OneOf = OneOf.seq(bg = white)(scenes)
  val menu: Glyph = FixedSize
    .Row(screenWidth, align=Mid)(
      FramedButton(" ← ") { _ => oneOf.prev() },
      FramedButton(" → ") { _ => oneOf.next() },
      FixedSize
        .Space(0f, 1f), // flexible space => right-justify the Help button
      FixedSize
        .Space(0f, 1f), // flexible space => right-justify the Help button
      Row(align=Mid)(for {i <- 0 until oneOf.length} yield sceneButton(i)),
      FixedSize
        .Space(0f, 1f), // flexible space => right-justify the Help button
      FramedButton(" Help ") { _ =>
        import windowdialogues.Dialogue
        Dialogue.OK(helpGUI, atPopupAnchor).start()
      },
      popupAnchor
    )
  private val oneScene = false
  val root: Glyph =
    (oneScene match {
      case true => Col.centered(scene0)
      case false =>
        Col.centered(
          Framed()(menu),
          Polygon(screenWidth, 1f, fg = black)((0, 0), (screenWidth, 0)),
          oneOf,
          Label(" ")
        )
    }) enlarged 10f

  /** A centered column derived from the distinct lines in `text` */
  private def textColumn(font: Font = buttonFont, fg: Brush = blue, align: Alignment=Center)(
    text: String
  ): Glyph = {
    val rows = text.split('\n')
    val texts: Seq[Glyph] = (for {row <- rows} yield Text(row, font, fg = fg)).toSeq
    Col(align=align, bg=nothing)(texts)
  }

  private def medex = exg()

  // TODO: Grids/Uniformly-sized Glyph ;lists/ ...

  private def trdown = trup(red).rotated(1)

  private def trhover = trup(green).rotated(2)

  private def wobbly(brush: Brush) =
    brush().sliced(25.0f, 6.0f)

  private def wibbly(brush: Brush) =
    brush().sliced(4.0f, 5.5f)

  private def filledStar7(brush: Brush) = filledStargon(7, fg = brush)

  private def polyStar7(brush: Brush) = openStargon(7, fg = brush)

  private def sceneButton(i: Int): Glyph =
    Framed()(ReactiveGlyphs.TextButton(s"$i", blue) { _ => oneOf.select(i) })

  private object Toggles extends BooleanButton {
    override val bg: Brush = yellowHuge
    override val fg: Brush = blue

    override def buttonFont: Font = medFont
  }
}

object LargeTest extends Application {
  override val defaultIconPath: Option[String] = Some("./parrot.png")

  val title      = "LargeTest"
  val theGUI     = new LargeTestGUI {}
  val GUI: Glyph = theGUI.root

  override val installRootHandlers: Boolean = true

  override val onUnfocussed: EventKey => Unit = {
    case event => if (event.isPressed) {
      import io.github.humbleui.jwm.Key._ // this is inelegant, but there's no straightforward way to import all keys into GlyphTypes then re-export them
      event.getKey match {
        case LEFT  => theGUI.oneOf.prev()
        case RIGHT => theGUI.oneOf.next()
        case _ =>
      }
    }
  }
}
