package org.sufrin.glyph
package tests.demonstrationBook

import styled.{Book, BookSheet, RadioCheckBoxes, TextButton}
import NaturalSize.{transparent, Col, Grid, Row}
import unstyled.static.INVISIBLE
import GlyphTypes.Scalar
import unstyled.reactive.Reaction

import io.github.humbleui.skija.paragraph.Alignment
import org.sufrin.glyph.styled.windowdialogues.Dialogue
import org.sufrin.glyph.styles.decoration.{Blurred, Decoration, Shaded}
import org.sufrin.glyph.Brushes.black
import org.sufrin.glyph.unstyled.dynamic.OneOf


class ButtonStyles (implicit val style: BookSheet, implicit val translation: glyphXML.Translation) {
  implicit val styleSheet: StyleSheet = style.pageSheet
  import translation._
  val book = Book()
  val Page = book.DefinePage
  val anchor = INVISIBLE()

  /** A reaction used below that pops up a Dialogue on which `glyph` is shown. */
  def showSourceCode(source: Glyph): Reaction = {
    _ => styled.windowdialogues.Dialogue.OK(source.enlarged(20).edged().enlarged(20)).East(anchor).start()
  }

  locally {
    translation("CENTERCODE") = new glyphXML.Macro(
      <div width="$width(75em)" textFontFamily="Courier" ><row width="1*width"><fill/>&BODY;<fill/></row></div>
    )

    translation("SOURCECODE") = new glyphXML.Macro(
      <div width="$width(75em)">
        <CENTERCODE width="$width">
          &BODY0;
        </CENTERCODE>
        <fill/>
        &BODY1..;
      </div>
    )
  }

  Page("Sampler", "") {
    import styled.TextButton
    import styles.decoration.{Framed, RoundFramed}

    lazy val fg: Brush = Brush("darkGrey.16.square")
    lazy val bg: Brush = Brush("lightGrey")
    lazy val startBG, buttonBG: Brush = Brush("transparent")
    lazy val startFG, buttonFG: Brush = Brush("white")
    var enlarge: Scalar = 5f
    var radius: Scalar = 20f
    var blur: Scalar = 10f
    var spread: Scalar = 12f
    var delta: Scalar = 12f
    var belta: Scalar = 0f

    import styled.windowdialogues.Dialogue.FLASH

    def exemplar(id: String, caption: () => String, decor: () => Decoration): Glyph = {
      lazy val button: Glyph = TextButton(id)
      { _ =>
        val style = styleSheet.copy(buttonForegroundBrush=buttonFG, buttonBackgroundBrush=buttonBG, buttonDecoration = decor())
        lazy val dialogue: Dialogue[Unit] =
          FLASH(
            Col(align=Center)(
              styled.TextButton(id){_=> }(style),
              styled.Label(caption().replace(',', '\n'))
            )
          )
        dialogue.InFront(anchor).start()
      }(styleSheet.copy(buttonBackgroundBrush=startBG, buttonDecoration = decor()))//(styleSheet.copy(buttonDecoration = Framed(black(width=4), enlarge=20)))
      button
    }



    lazy val roundframed: Glyph =
      exemplar("RoundFramed(fg, bg, enlarge, radius)",
               ()=>s"$fg, $bg, $enlarge, $radius, buttonfg=$buttonFG, buttonbg=$buttonBG",
               ()=>RoundFramed(fg, bg, enlarge, radius))

    lazy val  framed: Glyph = exemplar("Framed(fg, bg, enlarge)", ()=>s"$fg, $bg, $enlarge, buttonfg=$buttonFG, buttonbg=$buttonBG", ()=>Framed(fg, bg, enlarge))

    lazy val shaded: Glyph = exemplar("Shaded(fg, bg, enlarge, delta)", ()=>s"$fg, $bg, $enlarge, $delta, buttonfg=$buttonFG, buttonbg=$buttonBG", ()=>Shaded(fg, bg,  enlarge, delta=delta, down=false))

    lazy val  blurred: Glyph =
      exemplar("Blurred(fg, bg, blur, spread, enlarge, belta)",
               ()=>s"$fg, $bg, $blur, $spread, $belta, buttonfg=$buttonFG, buttonbg=$buttonBG",
               ()=>Blurred(fg, bg,  blur, spread, belta, enlarge))

    def selector(caption: String, preferred: Scalar, choices: Scalar*)(action: Scalar=> Unit) : Glyph = {
      styled.Label(s" $caption: ").beside(chooser(choices, preferred)(action)).framed(black(width=4))
    }


    def chooser(numbers: Seq[Scalar], preferred: Scalar)(select: Scalar => Unit): Glyph = {
      val labels = numbers.map {
        d => styled.Label(f"${d}%2.2f")
      }
      val hintText=numbers.mkString(" ")
      val oneOf= OneOf.seq(align=Center, fg=transparent, bg=transparent)(labels)
      val next = styled.TextButton(">", hint=Hint(5, hintText)){ _ => oneOf.next(); select(numbers(oneOf.selection))  }
      val prev = styled.TextButton("<", hint=Hint(5, hintText)){ _ => oneOf.prev(); select(numbers(oneOf.selection))  }
      locally {
        var prefs = for { i <- 0 until numbers.length if numbers(i)==preferred } yield i
        for { pref <- prefs } oneOf._selection=pref
      }
      NaturalSize.Row(align=Mid)(prev.framed(black), oneOf, next.framed(black))
    }


    Col(align=Center)(
      <div width="60em" align="justify" parSkip="3ex">
        <p align="center">Button decoration.</p>
        <p>Select colours and properties, then use one of the four
          buttons below to bring up a window containing a new and documented
          example of the decoration style that embodies them all exactly.
        </p>
        <fill height="3ex"/>
      </div>,
      anchor,
      Grid(width=2, padx=20, pady=20)(
        framed,
        roundframed,
        shaded,
        blurred
      ),

      Grid(width=3, padx=20, pady=20)(
        selector("Enlarge", enlarge, 0.0f, 0.1f, 0.2f, 0.3f, 0.5f, 0.8f, 0.9f, 20.0f, 30.0f, 40.0f, 50f, 60f){ v=>enlarge=v },
        selector("Radius", radius, 0.0f, 0.02f, 0.04f, 0.06f, 0.1f, 0.12f, 0.14f, 0.16f, 0.2f, 0.3f, 0.5f, 0.8f, 0.9f, 20.0f, 30.0f, 40.0f, 50f, 60f){ v=>radius=v },
        selector("Delta", delta, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>delta=v },

        selector("Blur", blur, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>blur=v},
        selector("Spread", spread, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>spread=v },
        selector("Belta", belta, 0, 2, 4, 6, 8, 10, 12, 14, 16, 20){ v=>belta=v },
      ),

      Grid(width=2, padx=20, pady=10).rows(
        styled.Label("FG"), styled.Label("BG"),
        new BrushChooser(fg, fg, { _=> }).COLOURGUI,
        new BrushChooser(bg, bg, { _=> }).COLOURGUI,
        styled.Label("buttonFG"), styled.Label("buttonBG"),
        new BrushChooser(buttonFG, buttonFG, { _=> }).COLOURGUI,
        new BrushChooser(buttonBG, buttonBG, { _=> }).COLOURGUI
      )
    )
  }

  Page("Examples", "") {
    import styled.TextButton
    import styles.decoration.{Framed, RoundFramed}

    def roundframed(fg: String, bg: String, radius: Scalar, buttonBG: String="lightGrey"): Glyph = {
      val button = TextButton(s"RoundFramed($fg, $bg, $radius)", Hint(10.0, s"buttonBackgroundBrush=$buttonBG"))
      { showSourceCode(
        <div align="left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>buttonDecoration = RoundFramed({fg}, {bg}, enlarge=30f, radius={radius.toString})</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=Brushes(buttonBG),
        buttonDecoration = RoundFramed(Brushes(fg), Brushes(bg), radius=radius, enlarge=30f)))
      button
    }

    def framed(fg: String, bg: String, buttonBG: String="lightGrey"): Glyph =
      TextButton(s"Framed($fg, $bg)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG"))
      { showSourceCode(
        <div align="Left" width="75em" DEC="Undecorated">
          <p>buttonBackgroundBrush={buttonBG}</p><p>Framed({fg}, {bg}, enlarge=35f)</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=Brushes(buttonBG),
                        buttonDecoration = Framed(Brushes(fg), Brushes(bg), enlarge=35f)))

    Col(align=Center)(
      <div width="60em" align="justify" parSkip="3ex">
        <p align="center">Button decoration with Framed and RoundFramed.</p>
        <p>Click or hover over any of the buttons to see detail, and to understand
           the relationship between button backgrounds and frame backgrounds. Unless "transparent"
           a button background has higher priority than the background of its frame.
        </p>
        <fill height="3ex"/>
      </div>,
      anchor,
      Grid(width=3, padx=10, pady=5).rows(
        framed("darkGrey.2.dashed(10,10)", "transparent", "transparent"), framed("darkGrey.2.sliced(5,2)", "transparent", "transparent"), framed("darkGrey.30.square", "transparent", "yellow"),
        framed("darkGrey.10.round", "transparent") ,  framed("darkGrey.20.round", "yellow") ,  framed("darkGrey.30.round", "transparent"),
        framed("darkGrey.10.square", "transparent") ,  framed("darkGrey.20.square", "yellow") ,  framed("darkGrey.30.square", "transparent"),

        roundframed("transparent", "lightGrey", 10, "transparent") ,  roundframed("transparent", "lightGrey.rounded(10)", 20, "green") ,  roundframed("transparent", "lightGrey", 30, "green"),
        roundframed("red.30", "yellow", 0.1f, "yellow") ,  roundframed("red.20", "yellow", 0.1f, "yellow") ,  roundframed("red.30", "yellow", 0.1f, "yellow"),
        roundframed("red.10", "green", 0.3f, "yellow") ,  roundframed("red.20", "green", 0.3f, "yellow") ,  roundframed("red.30", "green", 0.3f, "yellow"),
        roundframed("red.10", "transparent", 0.6f, "transparent") ,  roundframed("red.20", "yellow", 0.6f, "yellow") ,  roundframed("red.30", "yellow", 0.9f, "yellow"),
      )
    )
  }


  if (false) Page("Blurred/Shaded"){
    import styles.decoration.{Blurred, Shaded}

    def blurred(fg: String, bg: String, blur: Scalar, spread: Scalar, buttonBG: String="lightGrey", delta: Scalar=0f): Glyph =
      TextButton(s"Blurred($fg, $bg, $blur, $spread)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG\ndelta=$delta"))
      { showSourceCode(
        <div align="Left" width="75em">
          <p>decoration=Blurred({fg}, {bg}, blur={blur}, spread={spread}, delta={delta})</p>
          <p>buttonBackgroundBrush={buttonBG}</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=Brushes(buttonBG),
        buttonDecoration = Blurred(Brushes(fg), Brushes(bg), blur, spread, delta=delta, enlarge=20)))

    def shaded(fg: String, bg: String, down: Boolean, buttonBG: String="lightGrey", delta: Scalar=10f): Glyph =
      TextButton(s"Shaded($fg, $bg, ∂=$delta, down=$down)", hint=Hint(10.0, s"buttonBackgroundBrush=$buttonBG\ndelta=$delta"))
      { showSourceCode(
        <div align="Left" width="75em">
          <p>decoration=Shaded({fg}, {bg}, enlarge=25, delta={delta}, down={down})</p>
          <p>buttonBackgroundBrush={buttonBG}</p>
        </div>)
      }(styleSheet.copy(buttonBackgroundBrush=Brushes(buttonBG),
        buttonDecoration = Shaded(Brushes(fg), Brushes(bg), enlarge=20, delta=delta, down=down)))

    Col(align=Center)(
        <div width="65em" enlarged="20px">
            <p>Click/hover over any of the buttons to see detail, and to understand
              the relationship between button backgrounds and frame backgrounds. Unless "transparent"
              a button background has higher priority than the background of its frame.
            </p>
        </div>,
        Grid(width=3, padx=30, pady=5).rows(
            blurred("red",    "transparent",  10f, 5f, "transparent"),
            blurred("red",    "red",      20f, 5f, "transparent"),
            blurred("red",    "red",      20f, 20f, "green"),

            blurred("red",    "transparent",  10f, 5f, "transparent", 10),
            blurred("red",    "red",      20f, 5f, "transparent", 12),
            blurred("red",    "red",      20f, 20f, "green",  12),

          shaded("black", "transparent", true, "lightgrey", 5),
          shaded("black", "transparent", true, "lightgrey", 10),
          shaded("black", "transparent", true, "lightgrey", 15),

          shaded("black", "transparent", false, "lightgrey", 5),
          shaded("black", "transparent", false, "lightgrey", 10),
          shaded("black", "transparent", false, "lightgrey", 15),

          shaded("blue", "lightgrey", false, "transparent", 5),
          shaded("blue", "lightgrey", false, "green", 10),
          shaded("blue", "lightgrey", false, "transparent", 15),
        )
    )
  }

  //Page("Hints", "") {
  //  import styles.decoration.Shaded
  //}


  val GUI: Glyph = book.Layout.leftCheckBoxes()
}
