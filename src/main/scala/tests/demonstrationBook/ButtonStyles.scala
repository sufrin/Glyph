package org.sufrin.glyph
package tests.demonstrationBook

import styled.{Book, BookSheet, TextButton}
import NaturalSize.{Col, Grid}
import unstyled.static.INVISIBLE
import GlyphTypes.Scalar
import unstyled.reactive.Reaction


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

  Page("Framed", "") {
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
        framed("darkGrey.2.--", "transparent", "transparent"), framed("darkGrey.2.~~(5,2)", "transparent", "transparent"), framed("darkGrey.33", "transparent", "yellow"),
        framed("darkGrey.10.round", "transparent") ,  framed("darkGrey.20.round", "yellow") ,  framed("darkGrey.30.round", "transparent"),
        framed("darkGrey.10.square", "transparent") ,  framed("darkGrey.20.square", "yellow") ,  framed("darkGrey.30.square", "transparent"),

        roundframed("transparent", "lightGrey", 10, "transparent") ,  roundframed("transparent", "lightGrey.rounded(10)", 20, "green") ,  roundframed("transparent", "lightGrey", 30, "green"),
        roundframed("red.30", "yellow", 0.1f, "yellow") ,  roundframed("red.20", "yellow", 0.1f, "yellow") ,  roundframed("red.30", "yellow", 0.1f, "yellow"),
        roundframed("red.10", "green", 0.3f, "yellow") ,  roundframed("red.20", "green", 0.3f, "yellow") ,  roundframed("red.30", "green", 0.3f, "yellow"),
        roundframed("red.10", "transparent", 0.6f, "transparent") ,  roundframed("red.20", "yellow", 0.6f, "yellow") ,  roundframed("red.30", "yellow", 0.9f, "yellow"),
      )
    )
  }

  Page("Blurred/Shaded"){
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
