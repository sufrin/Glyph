package org.sufrin.glyph
package tests

import unstyled.static._
import GlyphTypes._
import Location._
import unstyled.windowdialogues.Dialogue.OK
import PolygonLibrary._
import styled.TextToggle
import unstyled.{reactive, static, BooleanButton, Text}

import io.github.humbleui.jwm.EventKey
import io.github.humbleui.skija.{BlendMode, PaintMode}
import org.sufrin.glyph.NaturalSize.Col
import org.sufrin.glyph.tests.Example2.font
import org.sufrin.glyph.GlyphTransforms.Enlarged

object LargeTestGUI {

  import unstyled.dynamic.OneOf
  import GlyphTransforms.Framed
  import NaturalSize.{Col, Row}
  import unstyled.reactive.{FramedButton, RawButton, ShadedButton}
  import Brushes._

  implicit val sheet: StyleSheet = StyleSheet()

  val  red = Brush("red.1.fill")
  val  redFrame = Brush("red.3.stroke")

  private lazy val atPopupAnchor = East(popupAnchor)

  val family = FontFamily("Menlo")
  //val face: Typeface = FontManager.default.matchFamilyStyle("Menlo", FontStyle.NORMAL) // old-style
  private val medFont: Font = family.makeFont(size=36) //new Font(face, 36)
  private val smallFont: Font = family.makeFont(size=20)//new Font(face, 20)
  private val largeFont: Font = family.makeFont(size=40)//new Font(face, 40)
  private val hugeFont: Font = family.makeFont(size=50)//new Font(face, 50)
  private val giantFont: Font = family.makeFont(size=75)//new Font(face, 75)
  private val buttonFont: Font = family.makeFont(size=28)//new Font(face, 28)
  private val exg = Text(" ", medFont)

  private val trup =
    FilledPolygon(100, 100, fg = blue)((100, 0), (0, 0), (100, 100), (100, 0))

  private val scene1 = {
    Col(align=Center)(
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

    Col(align=Center)(
      textColumn(fg = blue)("(2) framed raw button b() transformed by Rotated, Turned, and Scaled\nIntrinsic and extrinsic transforms behave identically"),
      medex,
      medex,
      Row(align=Mid)(
        Label("b().rotated(i)"),
        medex,
        Row(align=Mid)(for {i <- 0 until 4} yield b().rotated(i)),
        medex,
        medex,
        Label("Rotated(i)(b())"),
        medex,
        Row(align=Mid)(for {i <- 0 until 4} yield Rotated(i)(b()))
      ),
      medex,
      medex,
      Col(align=Center)(
        Label("b().scaled(5/i)"),
        medex,
        Row(align=Mid)(for {i <- 1 to 5} yield b().scaled(5f / i.toFloat))
      ),
      medex,
      medex,
      Col(align=Center)(
        Label("Scaled(5/i)(b())"),
        medex,
        Row(align=Mid)(for {i <- 1 to 5} yield Scaled(5f / i.toFloat)(b()))
      ),
      medex, medex,
      Col(align=Center)(
        Label("b().turned(i*23)"),
        medex,
        Row(align=Mid)(for {i <- 1 to 9} yield b().turned(i*23f))
      )
    )
  }

  private val scene3 = {
    def abcd(fg: Brush) =
      Row(align=Mid)(Label("A ")(fg), Label("B ")(fg), Label("C")(fg))

    def g(fg: Brush) = abcd(fg = fg)

    def b() = RawButton(g(blue(width=2)), g(red(width=2)), g(green(width=2))) { _ => }.framed(redFrame)

    Col(align=Center)(
      textColumn(fg = blue)("(3) Synthesised raw button b() transformed by \n.rotated(i), .turned(27f * i), and .scaled(1.5f) for i<-0 until 9"),
      medex,
      medex,
      Label("b().rotated(i).scaled(1.5)"),
      medex,
      Row(align=Mid)(for {i <- 0 until 9} yield b().rotated(i).scaled(1.5f)),
      medex,
      medex,
      Label("b().turned(27f * i)"),
      medex,
      Row(align=Mid)(for {i <- 0 until 9} yield b().turned(27f*i)),
      medex,
      medex,
      Label("b().turned(27f * i).scaled(1.5f)"),
      medex,
      Row(align=Mid)(for {i <- 0 until 9} yield b().turned(27f*i).scaled(1.5f)),
      medex,
      medex,
      Label("b().scaled(1.5f).turned(27f * i)"),
      medex,
      Row(align=Mid)(for {i <- 0 until 9} yield b().scaled(1.5f).turned(27f*i)),
    )
  }

  /** An invisible glyph that will act as the anchor for popups.
   * It will be placed at the (right) end of the menu bar.
   */
  private val popupAnchor: Glyph = INVISIBLE()

  /** A small but important test of placing reactives in `OneOf`s */
  private val scene4 = (
    Col(align=Center)(
      textColumn(fg = red)(
        "(4) Sub-interfaces can inhabit a OneOf\nShaded buttons appear to move when pressed\n[this entire app is structured as a OneOf, and gave rise to the Book() component]"
      ),
      medex,
      Col(align=Center)(
        ShadedButton("Press me [ShadedButton 1]") { _ =>
          import styled.windowdialogues.Dialogue
          Dialogue
            .OK(
              textColumn(smallFont)("You pressed shaded button 1"),
              atPopupAnchor
            )
            .start()
        },
        ShadedButton("Press me [ShadedButton 2]") { _ =>
          import org.sufrin.glyph.unstyled.windowdialogues.Dialogue
          Dialogue
            .OK(
              textColumn(smallFont)("You pressed shaded button 2"),
              atPopupAnchor
            )
            .start()
        },
        ShadedButton("[ShadedButton 3]") { _ =>
          import org.sufrin.glyph.unstyled.windowdialogues.Dialogue
          Dialogue
            .OK(
              textColumn(smallFont)("You pressed shaded button 3 in scene 4"),
              atPopupAnchor
            )
            .start()
        },
        ShadedButton("[ShadedButton 4]") { _ =>
          import org.sufrin.glyph.unstyled.windowdialogues.Dialogue
          Dialogue
            .OK(
              textColumn(smallFont)("You pressed shaded button 4 in scene 4"),
              atPopupAnchor
            )
            .start()
        }
      ) scaled 1.5f, medex scaled 3,
        textColumn(fg = blue)("(4a) Experiments with the .rounded(radius) and .dashed(onoffspecs) brush transforms\n(various strokewidths)"), medex,
      static.FilledRect(w=150,h=140, fg=blue.rounded(30f)) beside static.FilledRect(w=150,h=140, fg=red.rounded(50f)) beside static.FilledRect(w=150,h=140, fg=green.rounded(70f))
      , medex,
      static.Polygon(w=150,h=50, fg=blue.dashed(15f, 15f).strokeWidth(15f))((0,25f), (150f,25f)).framed() beside
      static.Polygon(w=150,h=50, fg=red.dashed(15f, 15f).strokeWidth(35f))((0,25f), (150f,25f)).framed() beside
      static.Polygon(w=150,h=50, fg=green.dashed(40f, 15f).strokeWidth(40f))((0,25f), (150f,25f)).framed() beside
      static.Polygon(w=250,h=50, fg=blue.dashed(40f, 40f).strokeWidth(40f).cap(ROUND))((0,25f), (250f,25f)).framed(), medex,
      medex,
    ).framed().enlarged(30f)
  )


  private val scene5 = {

    import unstyled.BooleanGlyphs._

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
      onOff(initially = false, fg = red, bg = white, NoHint) { _ =>
        toggle2.invert(); toggle3.invert(); toggle4.invert();
      }

    lazy val toggle2: OnOffButton =
      onOff(initially = true, fg = red, bg = white, NoHint) { _ =>
        toggle3.invert(); toggle4.invert();
      }

    lazy val toggle3: OnOffButton =
      onOff(initially = false, fg = red, bg = white, NoHint) { _ =>
        toggle4.invert();
      }

    lazy val toggle4: OnOffButton = onOff(
      trdown(red),
      trup(green),
      initially = true,
      fg = red,
      bg = white,
      NoHint
    ) { _ => toggle0.invert() }

    def t(name: String)(toggle: OnOffButton): Glyph =
      Row(align=Mid)(textColumn()(name), toggle.scaled(0.75f))

    val allOn = reactive.FramedButton("All on", fg = green) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.set(true)
      }
    }

    val allOff = reactive.FramedButton("All off", fg = red) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.set(false)
      }
    }

    val allFlip = reactive.FramedButton("Flip all", fg = blue) { _ =>
      for {
        t: OnOffButton <- List(toggle0, toggle1, toggle2, toggle3, toggle4)
      } {
        t.invert()
      }
    }

    def fill = FixedSize.Space.fill

    val caption = textColumn()(
      "(5) Five dependent toggles with states shown in various ways.\nFlipping each causes others to change state."
    )

      Col(align=Center)(
        caption,
        medex,
        FixedSize
          .Row(caption.w)
          .Mid(
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
        Row(align=Mid)(allOff, allFlip, allOn)
      ).enlarged(30f).framed(redFrame)
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
        import org.sufrin.glyph.unstyled.windowdialogues.Dialogue
        Dialogue
          .OK(
            Label(s"You typed $s"),
            RelativeTo(anchor1),
            "Report from TextField"
          )
          .start()
      },
      onCursorLeave = {
        _ => anchor1.guiRoot.giveupFocus()
      }
    )

    Col(align=Center)(
      Label("(6) TextField: a textlayout-entry field"),
      medex,
      medex,
      medex,
      SimpleParagraphs(70, align=Left)(
        """When the mouse pointer is NOT the within the text field border the Left and Right keys cause
          |the previous (next) page of the demonstration to be shown.
          |
          |When the mouse pointer is within the text field border, the cursor (a bold I-Beam) is always kept in view, and
          |the following additional keystrokes function as expected:
          |
          |[*] Ctrl/Cmd C - copy all to the clipboard
          |[*] Ctrl/Cmd X - cut all to clipboard
          |[*] Ctrl/Cmd V - insert the content of the clipboard
          |[*] Home/End/Left/Right - move the text cursor within the text
          |[*] Backspace - delete the character to the left of the cursor
          |[*] Delete - deletes the character to the right of the cursor and this is what happens to a long para
          |
          |
          |""".stripMargin), medex,
      theText.framed(blackFrame),
      Label(""),
      ShadedButton(
        "OS/X Symbols palette (double-click on a symbol to insert it)"
      ) { mods: Modifiers.Bitmap =>
        import Modifiers.{Middle, Secondary}
        if (mods.includeSome(Secondary | Middle))
          OK(
            Col(align=Center)(
              Label("Symbols palette not available (simulated error)")
            ),
            RelativeTo(anchor1),
            "Unimplemented"
          ).start()
        else {
          theText.takeKeyboardFocus()
          try io.github.humbleui.jwm.App.openSymbolsPalette()
          catch {
            case _: Throwable =>
              OK(
                Col(align=Center)(Label("Symbols palette not available")),
                RelativeTo(anchor1),
                "Unimplemented"
              ).start()
          }
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
    val giant = Text("Giant∑y", giantFont)
    val sp = FixedSize.Space(Text(" ", smallFont).w, 0f, 0f)
    def blob = Row(Baseline)(sp, FilledRect(3f, huge.h, fg=red).withBaseline(huge.baseLine), sp)
    import FixedSize.Space.fill


    val texts = List(fill, large, med, small, huge, fill)

    def atTop       = NaturalSize.Row(align=Top)(texts.map(_.copy(fg=red)))
    def atMid       = NaturalSize.Row(align=Mid)(texts.map(_.copy(fg=green)))
    def atBottom    = NaturalSize.Row(align=Bottom)(texts.map(_.copy(fg=blue)))
    def atBaseline  = NaturalSize.Row(align=Baseline)(texts.map(_.copy(fg=black))).showingBaseline



    locally { ShowingBaseline.fg = red.dashed(10,4) }

    Col(align=Center)(
      textColumn()(
        """(7) Various NaturalSize.Row(align=...) alignments
          |[Top(red), Mid(green), Bottom(blue), Baseline(black)]
          |showing baseline+frame in dashed red explicitly when a baseline is present.
          |The tall vertical bar has the height and baseline of the Huge glyph.
          """.stripMargin
      ),
      TextToggle(whenFalse="Enable baseline displays", whenTrue="Disable baseline displays", initially = true){ state => ShowingBaseline.enabled.set(state)}.shaded(),
      medex, atTop.framed() beside  blob beside atMid.framed(),
      medex, atBottom.framed() beside blob beside atBaseline.showingBaseline,
      medex scaled 2,
      textColumn()("A Row(align=Baseline) of two Row(align=Baseline) with a huge dash between"),
      Row(Baseline)(atBaseline, Text("-", hugeFont).showingBaseline, Row(align=Baseline)(huge(),large(),med()).showingBaseline),
      medex, textColumn()("FixedSize.Row(align=Baseline)"),
      FixedSize.Row(width=atBaseline.w*1.3f, align=Baseline)(blob::texts.map(_.copy())).framed().showingBaseline,
      medex, medex,
      textColumn()("Stretchable spaces used as rules between glyphs"),
      { def rule = new FixedSize.Space(1, 0, 1, 0, transparent, black(width=2).dashed(5f, 5f), baseLine=large.baseLine)
        FixedSize.Row(width = atBaseline.w * 1.3f, align = Baseline)(large(), rule, med(), rule, rule, small(), rule, huge()).framed()
      }

    )
  }


  private val scene8 = {
    val fatYellow: Brush = Brush("yellow.40.stroke.round")
    val fatGreen: Brush = Brush("green.40.stroke.round")
    val fatRed: Brush = Brush("red.40.stroke.round")
    val glyph: Glyph = Text("RawButton", medFont)

    val b1 = RawButton(glyph(), red, green) { _ => }
    val b2 = RawButton(glyph().enlarged(50f), red, green) { _ => }.framed()
    val b3 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }.framed()
    val b4 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }
      .enlarged(50f, bg = red)
      .framed()
    val b5 = RawButton(glyph().framed(fatYellow), fatRed, fatGreen) { _ => }

    def sp = Label(" ")

    Col(align=Center)(
      Label("(8) Raw buttons from glyph=Text(\"RawButton\", medFont) (try moving/pressing the mouse on these)"),
      sp,
      Col(align=Center)(b1, Label(" RawButton(glyph,red, green)")),
      sp,
      Col(align=Center)(
        b2,
        Label(" RawButton(glyph.enlarged(50f), red, green).framed()")
      ),
      sp,
      Col(align=Center)(
        b3,
        Label(" RawButton(glyph.framed(fatyellow), fatred, fatgreen).framed()")
      ),
      sp,
      Col(align=Center)(
        b4,
        Label(
          " RawButton(glyph.framed(fatyellow), fatred, fatgreen).enlarged(50f, bg=red).framed()"
        )
      ),
      sp,
      Col(align=Center)(
        b5,
        Label(" RawButton(glyph.framed(fatyellow), fatred, fatgreen)")
      ),
      sp
    )
  }.scaled(0.9f)
  private val scene9 = {
    val sp = Text(" ", medFont)
    import GlyphTypes._

    def space = sp()

    val n = 0
    val button: Glyph = Text("Raw", medFont, blue)
    val raw = RawButton(
      button.rotated(n),
      button(red).rotated(n),
      button(green).rotated(n)
    ) { _ => }

    def but(rot: Int, scale: Scalar): Glyph = {
      Row(
        Col(align=Center)(
          Label(s".rot($rot).scaled($scale)"),
          raw().rotated(rot).scaled(scale)
        ),
        space
      )
    }

    def sbut(rot: Int, scale: Scalar): Glyph = {
      Row(
        Col(align=Center)(
          Label(s".scaled($scale).rot($rot)"),
          raw().scaled(scale).rotated(rot)
        ),
        space
      )
    }

    Col(align=Center)(
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
      space,
      Row(
        sbut(0, .75f),
        sbut(0, 1f),
        sbut(0, 1.3f)
      ),
      space,
      Row(
        sbut(2, .75f),
        sbut(2, 1f),
        sbut(2, 1.3f)
      ),
      Row(
        sbut(1, .75f),
        sbut(1, 1f),
        sbut(1, 1.3f)
      ),
      space,
      Row(
        sbut(3, .75f),
        sbut(3, 1f),
        sbut(3, 1.3f)
      ),
    )
  }

  private val scene10 = {
    val sp = Text(" ", hugeFont)

    def space = sp()

    val caption: Text = Text("Raw", medFont)

    def button(n: Int)(fg: Brush): Glyph =
      caption(fg).rotated(n).framed(redFrame)

    val raw = RawButton(button(1)(blue), button(3)(red), button(3)(green)) {
      _ =>
    }

    Col(align=Center)(
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
      Row(align=Mid)(
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
      ShadedButton(button(scale)(fg), fg=fg, bg=transparent, delta=6f){ _ => }

    def rawR(scale: Scalar) =
      RawButton(rect(scale)(blue), rect(scale)(red), rect(scale)(green)) { _ =>
      }

    def rowA(raw: Scalar => Glyph) = Row(align=Mid)(
      Label("(1.5) "),
      raw(1.5f),
      space,
      Label("(2.5) "),
      raw(2.5f),
      space.scaled(1.5f)
    )

    def rowB(raw: Scalar => Glyph) = Row(align=Mid)(
      Label("(.75) "),
      raw(.75f),
      space,
      Label("(1.0) "),
      raw(1.0f),
      space
    )

    Col(align=Center)(
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
      Row(align=Mid)(
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
    val space = Text(" ", medFont)

    def sp: Glyph = space()

    def sliver: Glyph = sp scaled 0.3f

    val text = Text("Text", medFont, fg = blue)
    val rawbut: Glyph = RawButton(text, text(red), text(green)) { _ => println(s"$text pressed")}

    def rotOfBut(q: Int): Glyph =
      Col(align=Center)(sliver, Label(s"But(Text).rotated($q).framed()") scaled 0.7f, sliver, (rawbut().rotated(q).framed()))

    def turnOfBut(degrees: Scalar): Glyph =
      Col(align=Center)(sliver, Label(s"But(Text).turned($degrees).framed()") scaled 0.7f, sliver, (rawbut().turned(degrees)).framed())

    def butOfTurn(degrees: Scalar): Glyph =
      Col(align=Center)(sliver, Label(s"But(Text.framed().turned($degrees))") scaled 0.7f, sliver, (rawbut().framed().turned(degrees)))

    def butOfRot(q: Int): Glyph = Col(align=Center)(
      Label(s"But(Text.rotated($q)).framed()") scaled 0.7f, sliver,
      RawButton(text.rotated(q), text(red).rotated(q), text(green).rotated(q)) {
        _ =>
      }.framed()
    )

    Col(align=Center)(
        textColumn(buttonFont, blue)("""(12) Testing reactive-tracking under geometry transforms."""), medex, medex,

        textColumn(buttonFont, blue)("""Buttons of a pre-rotated text glyph."""),
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
        )
    )
  }

  private val scene13: Glyph = {
    import GlyphTransforms.Enlarged
    Col(align=Center)(
      Label("(13) Tracking reactive glyphs when there are many visible"),
      medex,
      Col(align=Center)(
        for {j <- 0 until 12} yield Row(align=Mid)({
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
          fg = red(width = 2, cap = Brushes.SQUARE)
        ) @@ Vec(x, 0),
        Rect(
          2,
          t.height,
          fg = blue(width = 2, cap = Brushes.SQUARE)
        ) @@ Vec(x + 4, 0),
        Rect(
          2,
          t.descent,
          fg = green(width = 2, cap = Brushes.SQUARE)
        ) @@ Vec(x + 8, t.height),
        Rect(
          t.width,
          2,
          fg = black(width = 1, cap = Brushes.SQUARE, alpha = 0.25f)
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

    val textA = Text("Á y", giantFont)
    val textG = Text("g Å", giantFont)
    val bodyA = textA(green)
    val bodyG = textG(green)

    Col(align=Center)(
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

    Col(align=Center)(
      Label("(15) An image synthesised as a glyph"),
      medex,
      new static.Image(flag scaled 1.2f)
    )
  }
  val bb: Brush = Brush("blue")(color = 0xff0000ff, width = 1.0f, cap = ROUND)
  val cc: Brush = bb(width = 15f, tag = "wide blue")
  private val scene16 = {

    val starSize = polyStar7(black).diagonal

    Col(align=Center)(
      textColumn(fg = blue)(
        "(16) Brushes with path effects\n(including .rounded, .dashed, .blurred, and .sliced)"
      ).enlarged(35).framed(wibbly(redFrame(width = 16f))),
      medex,
      NaturalSize.Grid(width=4)(
        FilledRect(starSize.x, starSize.y, red),
        FilledRect(starSize.x, starSize.y, red.rounded(starSize.x)),
        filledStar7(red),
        filledStar7(wobbly(blue)),
        polyStar7(wibbly(blue)),
        polyStar7(redLine),
        polyStar7(redLine.dashed(15f, 10f)),

        FilledRect(starSize.x, starSize.y, red.blurred(20f)),
        FilledRect(starSize.x, starSize.y, red.blurred(40f)),
        filledStar7(red.blurred(10f)),
        filledStar7(wobbly(blue).blurred(20f)),
        filledStar7((blueLine.blurred(30f))),
        filledStar7((blueLine.blurred(40f))),
      ) scaled 0.75f,
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
    Col(align=Center)(
      textColumn()("(17) Polygons and Stargons"),
      medex,
      filledStar7(wobbly(blue)),
      medex,
      Row(align=Mid)(gons.map(_.scaled(0.5f))),
      medex,
      Row(align=Mid)(fgons.map(_.scaled(0.5f))),
      medex,
      Row(
        Concentric.Center(sides.map { n: Float =>
          regularPolygon(n.toInt, fg = red(width = 1, alpha = 3f / n))
        }),
        medex,
        Concentric.Center(sides.map { n: Float =>
          regularPolygon(n.toInt, fg = blue(width = 3, alpha = 3f / n))
        })
      )
    )
  }

  private val scene18 = {
    import styled._
    implicit val sheet: StyleSheet = StyleSheet()
    import sheet.{em, ex}

    val starSize=filledStar7(transparent).diagonal
    val scale=starSize.y / trup.h

    Col(align=Center)(
      Label("(18) Some homebrewed simple buttons.\nAll are framed to show their extent."),
      ex, ex,
      TextButton("This is a button that has been framed()") { state =>
        println(s"TextButton $state")
      } . framed(),
      ex, ex,
      Label("These two GlyphButtons each show different shapes"),
      Label("when up, when hovering, and when pressed\n(responding to hovering/pressing requires the cursor to be within the showing shape)."),
      ex,ex,
      GlyphButton(trup.scaled(scale), trdown.scaled(scale), trhover.scaled(scale)) { state =>
        println(s"GlyphButton $state")
      } .framed() beside ex beside
      GlyphButton(
        filledStar7(wobbly(blue)),
        filledStar7(red) rotated 2,
        filledStar7(wobbly(green)) rotated 1
      ) { state =>
        println(s"GlyphButton $state")
      } .framed(),
      ex, ex,
      Label("Three ColourButtons\nThe second changes background when the mouse is hovering/pressed"),
      ex, ex,
      Row(align=Mid)(
        reactive.ColourButton(filledStar7(wobbly(blue)), red, green, background = false, NoHint){ _ => } . framed(), em,
        reactive.ColourButton(filledStar7(wobbly(blue)), red, green, background = true, NoHint){ _ => } . framed(), em,
        reactive.ColourButton("A ColourButton with a wobbly Frame", blue(width=0), red, green, background = false, hint=NoHint){ _ => }.enlarged(4).framed(wobbly(blueFrame)),
      )
    )
  }

  import PolygonLibrary._
    private val scene19 = {
      import styled._
      implicit val sheet: StyleSheet = StyleSheet()
      import sheet.ex

      val upColor     = Brush("pink")
      val downColor   = red(width = 0)
      val hoverColor  = green(width = 0)
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

      Col(align=Center)(
        Label(
          "(19) GlyphButtons made with polygonal glyphs\ndemonstrating contrasts in reactions to hovering/pressing"
        ),
        ex,
        Row(align=Mid)(
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
        }.framed().scaled(.75f),
        ex,
        Label(
          "GlyphButton made with framed polygonal glyphs\n(the cursor need only enter the bounding box)"
        ),
        ex,
        GlyphButton(
          filledStar7(upColor).enlarged(20).framed(),
          filledStar7(downColor).rotated(2).enlarged(20).framed(),
          filledStar7(hoverColor).rotated(1).enlarged(20).framed(),
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
    import unstyled.reactive.{TextButton => But}

    val fatYellow: Brush = yellowHuge.copy strokeWidth 40
    val fatGreen: Brush = fatYellow.copy col 0xff00ff00
    val fatRed: Brush = fatYellow.copy col 0xffff0000

    val title = textColumn(smallFont, black)("(20) Reactive elements within transformed rows/columns.\nNote that framing by .framed never requires a .enlarge\nbut using .framed sometimes does")

    def Tb(title: String) = But(title) { _ => println(s"Button $title") }

    def b1 = Tb("b1")

    def b2 = Tb("b2")

    def b3 = Tb("b3").scaled(1.2f)

    def b4 = Tb("b4").scaled(1.2f)

    val buttons = List(b1, b2, b3, b4)
    val reddish = redFrame//.copy(width = 0)
    val blueish = blueLine//.copy(width = 0)
    val greenish = greenLine//.copy(width = 0)
    val triv = false

    def separator: Rect = {
      Rect(100, 20, transparent)
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
      Col(align=Center)(
        title, separator,
        Row(frame = reddish, skip = 2f, uniform = true)(
          Col(frame = blueish)(b1, b2, b3, b4),
          Col(frame = blueish)(b2, b3),
          Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).framed(reddish)
          beside Label(".framed (reddish)") beside separator beside
          Row(frame = greenish, skip = 2f, uniform = true)(
            Col(frame = blueish)(b1, b2, b3, b4),
            Col(frame = blueish)(b2, b3),
            Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).enlarged(20).framed(reddish)
            beside Label(".enlarged(20).framed(reddish)"),
        separator,
        Row(align=Mid)(
          Row(frame = reddish, skip = 50f, uniform = true, align=Top)(
            Col(uniform = true, frame = blueish, align=Center)(b1, b2, b3, b4),
            Col(uniform = true, frame = blueish, align=Center)(b2, b3),
            Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).framed(reddish),
          separator,
          Row(frame = reddish, align=Top)(
            Col(uniform = true, frame = blueish, align=Center)(b1, b2, b3, b4),
            Col(uniform = true, frame = blueish, align=Center)(b2, b3).rotated(3),
            Col(uniform = true, frame = greenish)(b1, b3).framed(blueish).rotated(2),
            Col(uniform = true, frame = greenish)(b1, b3).framed(blueish).rotated(3)
          ).framed(reddish)
        ) beside separator beside
          Col(uniform = false, frame = reddish) (Row(frame = blueish, align=Mid)(b1, b2, b3, b4),
          Row(frame = blueish, align=Mid)(b2, b3),
          Col(uniform = true, frame = blueish)(b1, b3).framed(blueish)).framed(reddish)
      )
  }

  private val scene21 = {
    import org.sufrin.glyph.unstyled.reactive.{TextButton => But}

    val fatYellow: Brush = yellow(width=40)
    val fatGreen: Brush = green(width=40)
    val fatRed: Brush = red(width=40)

    val title = textColumn(smallFont, black)(
      """(21) Reactive elements within transformed grids/rows/columns.
        |
        |
        |""".stripMargin)

    val thinBlue = Brushes.blue(width = 4)

    def Tb(title: String) = But(title) { _ => println(s"Button $title") }

    def Rb(w: Scalar, h: Scalar, brush1: Brush = fatYellow, brush2: Brush = fatGreen, brush3: Brush = fatRed) = {
      val r = FilledRect(w, h, brush1) // Row(FilledRect(w, h, brush1), FilledRect(w/20, h, brush2))
      reactive.RawButton(r, r(brush2), r(brush3)) { _ => println(s"Button ($w,$h)") }.framed(thinBlue)
    }

    def Eb(w: Scalar, h: Scalar, brush1: Brush = fatYellow, brush2: Brush = fatGreen, brush3: Brush = fatRed) = {
      val r = FilledOval(w, h, brush1)
      reactive.RawButton.exact(r, r(brush2), r(brush3)) { _ => println(s"Button ($w,$h)") }.framed(thinBlue, bg = thinBlue)
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

    val column = Col(uniform = true, align=Center)(b1().enlargedTo(b1.w * 2, b1.h * 2), b2().rotated(1), b3().rotated(2)).framed(blueFrame)
    val rho = Row(uniform = true, align=Mid)(b1().enlargedTo(b1.w * 2, b1.h * 2), b2().rotated(1), b3().rotated(2)).framed(blueFrame)

    def separator: Rect = {
      Rect(100, 50, transparent)
    }

    val tT = false
    Col(align=Center)(
      title,
      separator,
      NaturalSize.Grid(padx = 30, pady = 30, fg = reddish, bg=lightGrey, width = 2).rows(
        textColumn(smallFont, black)("Turned 0.1,45,67.5,85.5"), textColumn(smallFont, black)("Rotated 0,1,2,3"),
        Row()(t1().turned(0.1f, tT).framed(), t1().turned(45f, tT).framed(),
          t1().turned(67.5f, tT).framed(), t1().turned(85.5f, tT).framed()),
        Row()(t1().rotated(0).framed(), t1().rotated(1).framed(), t1.rotated(2).framed(), t1().rotated(3).framed()),
        Row()(t2().turned(0.01f, tT).framed(), t2().turned(55f, tT).framed(),
          t2().turned(67.5f, tT).framed(), t2().turned(85.5f, tT).framed()),
        Row()(t2().rotated(0).framed(), t2().rotated(1).framed(), t2().rotated(2).framed(), t2().rotated(3).framed())) scaled .8f,
      separator scaled 0.5f,
      NaturalSize.Grid(padx = 30, fg = reddish, width = 2).rows(
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
          Row(align=Mid)(rho().mirrored(true, false), rho().skewed(-0.2f, 0f), rho().skewed(0.5f, 0f)),
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
      onOff(initially=false, fg=green, bg=red, NoHint) {
        state =>
          enableButton.guiRoot.autoScale = state
      }.framed(fg=red(width=2))
  }


  private val helpGUI =
    Col(align=Center)(
    SimpleParagraphs(ems=70, smallFont, blue)(
      """[C] This is a test of some basic Glyph  components.
        |
        |If its window doesn't fit on your screen, or if you'd like it to be bigger, first click the window
        |resizing checkbox to enable dynamic resizing; then drag one of the window edges. While you are dragging,
        |the window will be continuously resized (keeping the same aspect ratio) and its content rescaled accordingly.
        |
        |The test has a few  numbered scenes that can be accessed in rotation by typing one of the "←" and "→" keys,
        |or clicking one of the buttons at the left end of the menu bar or one of the numbered buttons.
        |
        |[C] ----------------
        |
        |[*] The scenes arose arose organically during development so some now appear redundant. We have nevertheless continued to
        |maintain the application because it is a good regression test for the most basic functions of the kit.
        |
        |[*] Elsewhere there are demonstrations of some of the derived GUI components, such as styled texts, menus, and popups.""".stripMargin
    )
  )

  private val scene0 = {
    Col(align=Center)(helpGUI)
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
      FixedSize.Fill(0f, 2f), // flexible space => right-justify the resizing button
      Row(align=Mid)(for {i <- 0 until oneOf.length} yield sceneButton(i)),
      FixedSize.Fill(0f, 1f), // flexible space => right-justify the resizing button
      Row(align=Mid, skip=10)(Label("Window resizing enabled "), Resizing.enableButton),
      popupAnchor
    )

  private val oneScene = false

  val root: Glyph =
    (oneScene match {
      case true => Col(align=Center)(scene0)
      case false =>
        Col(align=Center, skip=10)(
          menu,
          oneOf.framed(black),
        )
    }) enlarged 10f

  /** A column derived from the distinct lines in `text` */
  private def textColumn(font: Font = buttonFont, fg: Brush = blue, align: Alignment=Center)(
    text: String
  ): Glyph = {
    val rows = text.split('\n')
    val texts: Seq[Glyph] = (for {row <- rows} yield Text(row, font, fg = fg)).toSeq
    Col(align=align, bg=transparent)(texts)
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
    Framed(fg=black(width=0))(Enlarged(10,5)(reactive.TextButton(s"$i", blue) { _ => oneOf.select(i) }))

  private object Toggles extends BooleanButton {
    override val bg: Brush = yellowHuge
    override val fg: Brush = blue

    override def buttonFont: Font = medFont
  }
}

object LargeTest extends Application {
  override val defaultIconPath: Option[String] = Some("ICONS/parrot.png")

  val title      = "LargeTest"
    locally {
      println("Starting")
    }

  val GUI: Glyph = Col(align=Center)(
    LargeTestGUI.root
  )

  override def handleUnfocussedKey(event: EventKey): Unit = {
    if (event.isPressed) {
      import io.github.humbleui.jwm.Key._ // this is inelegant, but there's no straightforward way to import all keys into GlyphTypes then re-export them
      event.getKey match {
        case LEFT  => LargeTestGUI.oneOf.prev()
        case RIGHT => LargeTestGUI.oneOf.next()
        case key => println(key)
      }
    }
  }
}
