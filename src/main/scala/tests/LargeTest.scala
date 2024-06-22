package org.sufrin.glyph
package tests

import Glyphs._
import GlyphTypes._
import Location._
import Styles.GlyphStyle
import windowdialogues.Dialogue.OK
import PolygonLibrary._

trait LargeTestGUI extends Brushes {

  import DynamicGlyphs.OneOf
  import GlyphTransforms.{Framed, Scaled}
  import NaturalSize.{Col, Row}
  import ReactiveGlyphs.{FramedButton, RawButton, ShadedButton}

  private lazy val atPopupAnchor = East(popupAnchor)
  val face: Typeface =
    FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL)
  private val medFont: Font = new Font(face, 36)
  private val smallFont: Font = new Font(face, 20)
  private val largeFont: Font = new Font(face, 40)
  private val hugeFont: Font = new Font(face, 50)
  private val hugerFont: Font = new Font(face, 100)
  private val buttonFont: Font = new Font(face, 28)
  private val exg = Text(" ", medFont).asGlyph()
  private val trup =
    FilledPolygon(100, 100, fg = blue)((100, 0), (0, 0), (100, 100), (100, 0))
  private val scene1 = {
    Col.centered(
      textColumn(fg = blue)("(1) Primitive text column layouts"),
      medex,
      medex,
      textColumn(fg = red)("(TextColumn) A single line\n"),
      medex,
      textColumn(fg = blue)(
        "(TextColumn) A wide line\nabove a\ncouple more lines.\n"
      ),
      medex,
      textColumn(smallFont, fg = green)(
        "(TextColumn) A few more lines\nin a smaller font\njust to see whether TextColumn makes sense.\n"
      )
    )
  }
  private val scene2 = {
    import GlyphTransforms.{Rotated, Scaled}
    def abcd(fg: Brush) = Row(Label("A ")(fg), Label("B ")(fg), Label("C")(fg))
    def g(fg: Brush) = abcd(fg = fg).framed(fg)
    def b() = RawButton(g(blue), g(redFrame), g(greenFrame)) { _ => }
    Col.centered(
      textColumn(fg = blue)("(2) Framed: Rotations and Scalings commute"),
      medex,
      medex,
      Row.centered(
        Label("b().rotated(i)"),
        medex,
        Row.centered$(for { i <- 0 until 4 } yield b().rotated(i)),
        medex,
        medex,
        Label("Rotated(i)(b())"),
        medex,
        Row.centered$(for { i <- 0 until 4 } yield Rotated(i)(b()))
      ),
      medex,
      medex,
      Col.centered(
        Label("b().scaled(3/i)"),
        medex,
        Row.centered$(for { i <- 1 to 3 } yield b().scaled(3f / i.toFloat))
      ),
      medex,
      medex,
      Col.centered(
        Label("Scaled(3/i)(b())"),
        medex,
        Row.centered$(for { i <- 1 to 3 } yield Scaled(3f / i.toFloat)(b()))
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
      textColumn(fg = blue)("(3) Unframed: Rotations and Scalings commute"),
      medex,
      medex,
      Label("b().rotated(i).scaled(1.5)"),
      medex,
      Row.centered$(for { i <- 0 until 4 } yield b().rotated(i).scaled(1.5f)),
      medex,
      medex,
      Label("Rotated(i)(b())"),
      medex,
      Row.centered$(for { i <- 0 until 4 } yield Rotated(i)(b())),
      medex,
      medex,
      Label("Rotated(i)(b().scaled(1.5))"),
      medex,
      Row.centered$(for { i <- 0 until 4 } yield Rotated(i)(b().scaled(1.5f))),
      medex,
      medex
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
        "(4) Sub-interfaces can inhabit a OneOf\n[this entire app is structured as a OneOf]"
      ),
      medex,
      ShadedButton.ofString("Press me [StyledButton 1]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed nested button 1"),
            atPopupAnchor
          )
          .start()
      },
      ShadedButton.ofString("Press me [StyledButton 2]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed nested button 2"),
            atPopupAnchor
          )
          .start()
      },
      medex,
      medex,
      ShadedButton.ofString("[StyledButton 1]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed nested button 1 in scene 4"),
            atPopupAnchor
          )
          .start()
      },
      textColumn(smallFont)(" can be pressed, and so can  "),
      ShadedButton.ofString("[StyledButton 2]") { _ =>
        import windowdialogues.Dialogue
        Dialogue
          .OK(
            textColumn(smallFont)("You pressed nested button 2 in scene 4"),
            atPopupAnchor
          )
          .start()
      }
    ) enlarged 30f
  )
  private val scene5 = {

    import OnOffButton._

    import Toggles._

    lazy val toggle0: OnOffButton = onOff(
      "OnOffButton 0 is\nOff",
      "OnOffButton 0 is\nOn",
      fg = red,
      bg = white,
      initially = true
    ) { state =>
      toggle1.invert(); toggle2.invert(); toggle3.invert(); toggle4.invert()
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
      trdown(red)(bg = yellow),
      trup(green)(bg = yellow),
      initially = true,
      fg = red,
      bg = white
    ) { _ => toggle0.invert() }

    def t(name: String)(toggle: OnOffButton): Glyph =
      Row.centered(textColumn()(name), toggle.scaled(0.75f))

    val allOn = ReactiveGlyphs.FramedButton("All on", fg = green) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } { t.set(true) }
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

    Framed(redFrame)(
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
      ) enlarged 20f
    )
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
      ShadedButton.ofString(
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
    val sp = Text(" ", medFont)
    def space = sp.asGlyph()
    val texts = List(huge, sp, large, sp, med, sp, small, sp, huge)


    Col.centered(
      textColumn()(
        """(7) Showing the difference between making a Row.atTop of from .asGlyph
          |texts of different sides, and from the same .atBaseLine.
          |[We may eventually change the API to avoid the distinction.]
          |
          |The .asGlyph version.
          |""".stripMargin
      ),
      medex,
      medex,
      Row.atTop$(texts.map(_.asGlyph())),
      space,
      textColumn()("""The .atBaseLine version"""),
      medex,
      Row.atTop$(texts.map(_.atBaseline())),
      medex,
      textColumn()("""The .atBaseLine version (with baseline shown)"""),
      medex,
      Row.atTop$(texts.map(_.atBaseline())).$$$$()
    )
  }
  private val scene8 = {
    val fatYellow: Brush = yellow.copy strokeWidth 40
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
      textColumn(buttonFont, blue)("""(10) Scaling/rotation when making buttons.
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

    val sp = Text(" ", medFont)

    def space = sp.asGlyph()

    def button(scale: Scalar)(fg: Brush): Glyph =
      Text("StyledButton", medFont).asGlyph(fg).scaled(scale)
    def rect(scale: Scalar)(fg: Brush) = FilledRect(30, 30, fg).scaled(scale)

    def rawB(scale: Scalar) =
      RawButton(button(scale)(blue), button(scale)(red), button(scale)(green)) {
        _ =>
      }.framed()
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
      textColumn(smallFont, blue)("""(11) Making buttons of pre-scaled glyphs
          |""".stripMargin),
      space,
      rowA(rawB),
      rowB(rawB),
      rowA(rawR),
      rowB(rawR),
      space,
      textColumn(smallFont, red)(
        """Post-scaling buttons of the same glyphs could produce a slightly different appearance
          |when the button is framed; but the behaviour is the same""".stripMargin
      ),
      space,
      Row.centered(
        Label("(2.5)"),
        space,
        Scaled(2.5f)(rawB(1.0f)),
        space,
        Scaled(2.5f)(
          RawButton(
            FilledRect(30f, 30f, blue),
            FilledRect(30f, 30f, red),
            FilledRect(30f, 30f, green)
          ) { _ => }
        )
      )
    )
  }
  private val scene12 = {
    import GlyphTransforms.Rotated
    val space = Text(" ", medFont).asGlyph()
    def sp: Glyph = space()
    def sliver: Glyph = sp scaled 0.2f

    val text = Text("Text", medFont).asGlyph(fg = blue)
    val rawbut: Glyph = RawButton(text, text(red), text(green)) { _ => }

    def badRotOfBut(q: Int): Glyph =
      Col.centered(Label(s"Rot($q)(But(Text))"), Rotated(q)(rawbut()))

    def butOfRot(q: Int): Glyph = Col.centered(
      Label(s"But(Text.rot($q))"),
      RawButton(text.rotated(q), text(red).rotated(q), text(green).rotated(q)) {
        _ =>
      }
    )
    Col
      .centered(
        textColumn(buttonFont, blue)(
          """(12) Testing reactive-tracking under geometry transforms.
                    |
                    |Buttons of pre-rotated glyphs""".stripMargin
        ),
        sliver,
        Row(butOfRot(0), sp, butOfRot(2), sp, butOfRot(1), sp, butOfRot(3)),
        sp,
        sp,
        textColumn(buttonFont, blue)("Post-Rotated buttons of the same glyph"),
        Row(
          badRotOfBut(0),
          sp,
          badRotOfBut(2),
          sp,
          badRotOfBut(1),
          sp,
          badRotOfBut(3)
        ),
        sp
      )
      .enlarged(sp.w)
  }
  private val scene13: Glyph = {
    import GlyphTransforms.Enlarged
    Col.centered(
      Label("(13) Tracking reactive glyphs when there are many visible"),
      medex,
      Col.centered$(
        for { j <- 0 until 12 } yield Row.centered$({
          val buttons =
            for { i <- 0 until 13 } yield sceneButton(i).rotated((i + j) % 4)
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
          RawButton(bodyG(blue), bodyG(red), bodyG(yellow)) { _ => }
        )
      ),
      medex,
      Row(
        Label("RawButton.exact"),
        measure(textG, bodyG),
        measure(
          textG,
          RawButton.exact(bodyG(blue), bodyG(red), bodyG(yellow)) { _ => }
        )
      ),
      medex,
      Row(
        Label("RawButton.exact"),
        measure(textA, bodyA),
        medex,
        measure(textA, RawButton.exact(bodyA(blue), red, yellow) { _ => })
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

    Col.centered(
      textColumn(fg = blue)(
        "(16) Brushes with path effects\n(here two are shown)"
      ).enlarged(35).framed(wibbly(redFrame(width = 16f))),
      medex,
      Row(
        filledStar7(red),
        medex,
        filledStar7(wobbly(blue)),
        medex,
        polyStar7(wibbly(blue)),
        medex,
        polyStar7(redLine)
      ) scaled 0.75f,
      medex
    )
  }
  private val scene17 = {
    val sides = List(3f, 5f, 7f, 9f, 11f, 13f)
    val gons = sides.map { n: Float =>
      Concentric(
        Label(s"${n.toInt}", medFont),
        openStargon(n.toInt, R=200, C=200, fg = blue(width = 6), bg = red(alpha = 0.3f))
      )
    }
    val fgons = sides.map { n: Float =>
      Concentric(
        filledRegularPolygon(
          n.toInt,
          fg = yellow(width = 9),
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
    import styled._
    import styled.TextLayout._

    import BlueContext.Spaces._
    implicit val style: Styles.Sheet = BlueContext

    Col.centered(
      TextLabel("(18) Some styled glyphs with implicitly-specified properties"),
      ex,
      TextButton("This is a button") { state =>
        println(s"StyledButton $state")
      },
      ex,
      TextLabel("The buttons show different shapes"),
      TextLabel("when up, when hovering, and when pressed"),
      GlyphButton(trup, trdown, trhover) { state =>
        println(s"StyledButton $state")
      },
      ex,
      GlyphButton(
        filledStar7(wobbly(blue)),
        filledStar7(red) rotated 2,
        filledStar7(wobbly(green)) rotated 1
      ) { state =>
        println(s"StyledButton $state")
      },
      ex, ex,
      TextLabel("Two unstyled ColourButtons"),
      Row.centered(
          ReactiveGlyphs.ColourButton(filledStar7(wobbly(blue)), red, green, background = false){ _ => }, em,
          ReactiveGlyphs.ColourButton("A ColourButton with a wobbly Frame", blue(width=0), red, green, background = false){ _ => }.enlarged(4).framed(wobbly(blue)),
      )
    )
  }

  import PolygonLibrary._
  private val scene19 = {
    import styled._
    import styled.TextLayout._

    import BlueContext.Spaces._
    implicit val style: Styles.Sheet = BlueContext

    val upColor     = yellow(name = "yellow", width = 0)
    val downColor   = red(name = "red", width = 0)
    val hoverColor  = green(name = "green", width = 0)
    val noEffect    = hoverColor.pathEffect
    val wibEffect    = wibbly(green).pathEffect

    // Each of these checkboxes MUTATEs its associated colour
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
      TextLabel(
        "(19) GlyphButton of polygonal glyphs\n(the cursor must enter the shape)"
      ),
      ex,
      Row.centered(
        TextLabel("Enable wobbly paint for up:"),
        tUp,
        TextLabel(", down:"),
        tDown,
        TextLabel(", hover:"),
        tHover
      ),
      ex,
      GlyphButton(
        filledStar7(upColor),
        filledStar7(downColor)  rotated 2,
        filledStar7(hoverColor) rotated 1
      ) { _ =>
        overlaydialogues.Dialogue
          .OK(TextLabel("You pressed the top button"))
          .InFront(tHover)
          .start()
      }.scaled(.75f),
      ex,
      TextLabel(
        "GlyphButton of framed polygonal glyphs\n(the cursor need only enter the bounding) box"
      ),
      ex,
      GlyphButton(
        filledStar7(upColor).enlarged(20),
        filledStar7(downColor).enlarged(20),
        filledStar7(hoverColor).enlarged(20),
        exact = false
      ) { _ =>
        overlaydialogues.Dialogue
          .OK(TextLabel("You pressed the bottom button"))
          .InFront(tHover)
          .start()
      }.scaled(.75f)
    )
  }

  private val scene20 = {
    import ReactiveGlyphs.{ColourButton, TextButton}
    val fatYellow: Brush = yellow.copy strokeWidth 40
    val fatGreen: Brush = fatYellow.copy col 0xff00ff00
    val fatRed: Brush = fatYellow.copy col 0xffff0000
    val l1 = Label("ColourButton", fg=fatRed, bg=white)
    val b1 = new ColourButton(l1, fatYellow, fatGreen, react = { _ => }, background = false)
    val b2 = TextButton("TextButton", fg=fatRed, bg=white) { _ => }

    Col.centered(
       b1.framed(), b2.framed()
    )
  }

  private val helpGUI = Framed(whiteFrame)(
    textColumn(smallFont, black)(
      """This is a test of some basic GUI kit components.
      |
      |It has a few demonstration "scenes" that are accessed
      |in rotation by clicking one of the "←" and "→" buttons
      |at the left of the menu bar; or by clicking one
      |of the numbered buttons.
      |
      |The scenes arose arose organically during development
      |so some may now appear redundant.
      |
      |Elsewhere there are demonstrations of some of the derived GUI components,
      |such as styled texts, menus, and popups.""".stripMargin
    )
  )
  private val scene0 = { Col.centered(helpGUI) } // the preferred scene
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
    scene20
  )
  /** Width of the menu bar */
  private val screenWidth = scenes.map(_.w).max
  val oneOf: OneOf = OneOf.seq()(scenes)
  val menu: Glyph = FixedSize
    .Row(screenWidth)
    .atTop(
      FramedButton(" ← ") { _ => oneOf.prev() },
      FramedButton(" → ") { _ => oneOf.next() },
      FixedSize
        .Space(0f, 1f), // flexible space => right-justify the Help button
      FixedSize
        .Space(0f, 1f), // flexible space => right-justify the Help button
      Row.atTop$(for { i <- 0 until oneOf.length } yield sceneButton(i)),
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

  private def textColumn(font: Font = buttonFont, fg: Brush = blue)(
      text: String
  ): Glyph = {
    val rows = text.split('\n')
    val texts =
      (for { row <- rows } yield Text(row, font).asGlyph(fg = fg)).toList
    Col.centered$(INVISIBLE(fg = fg) :: texts)
  }

  private def medex = exg()

  // TODO: Grids/Uniformly-sized Glyph ;lists/ ...

  private def trdown = trup(red).rotated(1)

  private def trhover = trup(green).rotated(2)

  private def wobbly(brush: Brush) =
    brush().pathEffect(PathEffect.makeDiscrete(25.0f, 6.0f, 1))

  private def wibbly(brush: Brush) =
    brush().pathEffect(PathEffect.makeDiscrete(4.0f, 5.5f, 14))

  private def filledStar7(brush: Brush) = filledStargon(7, fg = brush)

  private def polyStar7(brush: Brush) = openStargon(7, fg = brush)

  private def sceneButton(i: Int): Glyph =
    Framed()(ReactiveGlyphs.TextButton(s"$i", blue) { _ => oneOf.select(i) })

  private object Toggles extends BooleanButton {
    override val bg: Brush = yellow
    override val fg: Brush = blue

    override def buttonFont: Font = medFont
  }

  /** This context is used in the next few scenes.
    * It's derived from
    */

  private object BlueContext extends Styles.Sheet{
    object Super extends Styles.Sheet
    override lazy val face: Typeface =
      FontManager.default.matchFamilyStyle("Courier", FontStyle.ITALIC)
    override implicit lazy val labelStyle: GlyphStyle =
      Super.labelStyle.copy(fg = blue, font = new Font(face, 26))
  }
  // last Label makes the window long enough
  // TODO: what depth is the top of the window frame?
  //       might we need to revert to stock swing/jdk components to deal with this issue
  //       or will JWM eventually support that functionality
}

object LargeTest extends Application {
  override val defaultIconPath: Option[String] = Some("./flag.png")
  val title = "LargeTest"
  val GUI: Glyph = new LargeTestGUI {}.root
}
