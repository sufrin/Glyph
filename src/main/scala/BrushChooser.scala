package org.sufrin.glyph

import styled.MenuButton
import unstyled.reactive.{Enterable, Reaction}

import org.sufrin.glyph.Brush.{BUTT, ROUND, SQUARE}
import org.sufrin.glyph.Brushes.{black, lightGrey, NonBrush}
import org.sufrin.glyph.Colour.{ARGB, HSV}
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}
import org.sufrin.glyph.NaturalSize.{transparent, Col, Grid, Row}
import org.sufrin.glyph.styles.decoration.Framed
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.styled.windowdialogues.Dialogue
import org.sufrin.logging

/**
 * Provides several `Glyph`s for use as user interfaces that support 
 * the assignment of brush properties to the `resultBrush: Brush`, whose
 * initial properties are those of `protoBrush: Brush`.
 * 
 * 
 */
class BrushChooser(val protoBrush: Brush, val resultBrush: Brush, val onError: NonBrush=>Unit, onChoose: Brush => Unit = {_ => })(implicit sheet: StyleSheet) {
  thisChooser =>
  
  protected def brushWarning(anchor: Glyph) = {
    import glyphXML.Language._
    styled.windowdialogues.Dialogue.FLASH(
      <p width="80em" leftMargin="1em" rightMargin="1em">
        The brush specification is erroneous. Specification syntax is:
        <![CDATA[
          *   specification ::= named [decoration]*
          *
          *   named         ::= 0Xaarrggbb   // 4 hex bytes: alpha, red, blue, green
          *                 |   hsv(#hue,#saturation,#brightness)
          *                 |   "one of the named colours"
          *
          *   decoration    ::=  #strokewidth)
          *                 |    .rounded(#strokeradius)
          *                 |    .dashed(#on, #off)
          *                 |    .sliced(#sliceLength,#maxdisplacement)
          *                 |    .stroke | .fill | .stroke&fill
          *                 |    .round | .butt | .square
          *                 |    .alpha(#alpha)
          *                 |    .blurred(#blur)]]>
      </p>).InFront(anchor)
  }

  protected val hintSheet: StyleSheet = sheet.copy(fontScale = 0.7f)
  protected val menuSheet: StyleSheet = hintSheet.copy(buttonDecoration = Framed(black(width=1.5f), enlarge=8), buttonBackgroundBrush = lightGrey)

  protected def setResultBrush(specification: String): Unit = {
    if (specification.nonEmpty) {
      try {
        val brush = Brushes.Parse(specification)
        resultBrush.copyFrom(brush)
        protoBrush.copyFrom(brush)
        showResultBrush()
      }
      catch {
        case nonBrush: Brushes.NonBrush =>
          onError(nonBrush)
      }
    }
  }

  protected def setResultBrush(brush: Brush): Unit = {
    resultBrush.copyFrom(brush)
    showResultBrush()
  }

  protected def showResultBrush(): Unit = {
    brushField.string = resultBrush.toString
    onChoose(resultBrush)
  }

  protected lazy val brushField = styled.TextField(size=25, onEnter=setResultBrush(_), initialText=resultBrush.toString)(hintSheet)

  protected def ColourPaletteButton(label: String, hint: String="", colour: Brush)(action: Reaction): Glyph = {
    val hover: Hint = if (hint.nonEmpty)  Hint(5, hint)(hintSheet) else NoHint
    unstyled.reactive.UniformlyColouredButton(Vec(sheet.emWidth*2, sheet.exHeight*2), colour, hover)(action)
  }

  protected def brushFieldChooserMenu(fieldname: String, choices: String*)(choose: String => Unit)(implicit sheet: StyleSheet): Glyph = {
    val buttons = choices.map {
      name => MenuButton(name){ _ => choose(name) }
    }
    val menu = styled.overlaydialogues.Dialogue.Menu(fieldname, nested=false, buttons)
    menu
  }


    import sheet.{em,ex}
    import GlyphShape._

  protected def brushFeedback(): Unit = setResultBrush(protoBrush.copy())
  
  protected   def rectangle(w: Scalar, h: Scalar)(brush: Brush): GlyphShape = {
      val path=new PathShape(brush, false)
      path.moveTo(0, 0)
      path.lineTo(w, 0)
      path.lineTo(w, h)
      path.lineTo(0, h)
      path.closePath
      path
    }

  protected val colButs = for { (name, col) <- Brushes.namedColours  } yield {
      val brush = Brushes(name)
      ColourPaletteButton("  ", f"$name%s (${brush.hue}%3.1f,${brush.sat}%1.1f,${brush.vib}%1.1f)", brush){
        _ =>
          protoBrush.setColor(col)
          brushFeedback()
          for { brush <- briBrushes++satBrushes } {
            brush.hue(ARGB(col).hue)
          }
      }
  }

  protected lazy val satBrushes: Seq[Brush] = for { i<- 0 to 10 } yield Brushes(s"hsv(${protoBrush.hue}, ${i*0.1}, 1)")
  protected lazy val briBrushes: Seq[Brush] = for { i<- 0 to 10 } yield Brushes(s"hsv(${protoBrush.hue}, 1, ${i*0.1})")

  protected lazy val satButs =
      for { i<-0 to 10 } yield  ColourPaletteButton(" ", f"Sat ${i*0.1}%1.1f", satBrushes(i)){
        _ =>
          //val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
          //setColor(HSV(h, i*0.1, v).toInt)
          protoBrush.sat(i*0.1)
          protoBrush.ComposeEffect.noFilter()
          brushFeedback()
          for { brush <- briBrushes } {
            brush.sat(i*0.1)
          }
      }

  protected lazy val briButs =
      for { i<-0 to 10 } yield  ColourPaletteButton(" ", f"Vib ${i*0.1}%1.1f", briBrushes(i)){
        _ =>
          //val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
          //protoBrush.setColor(HSV(h, s, i*0.1).argb)
          protoBrush.vib(i*0.1)
          protoBrush.ComposeEffect.noFilter()
          brushFeedback()
          for { brush <- satBrushes } {
            brush.vib(i*0.1)
          }
      }

  protected lazy val hueButs = for { i <- 0 until 24  } yield {
        ColourPaletteButton("  ", s"${i*15}°", Brushes(s"hsv(${i*15}).fill")){
          _ =>
            val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
            protoBrush.setColor(HSV(i*15, s, v).toInt)
            protoBrush.ComposeEffect.noFilter()
            brushFeedback()
            for { brush <- briBrushes++satBrushes } {
              brush.hue(i*15)
            }
        }
      }

  protected lazy val colourGrid = Grid(fg=black).table(width=12)(colButs.toList)
  protected lazy val hueGrid = Grid(fg=black).table(width=12)(hueButs.toList)
  protected lazy val satGrid = Grid(fg=black).table(width=11)(satButs.toList)
  protected lazy val briGrid = Grid(fg=black).table(width=11)(briButs.toList)
  protected lazy val textModel = unstyled.Text(" Text example ", sheet.labelFont.makeWithSize(36), protoBrush)

  protected lazy val helpDialogue: Dialogue[Unit] = {
    import glyphXML.Language._
    styled.windowdialogues.Dialogue.FLASH(<div width="50em" align="justify" parskip="1.5ex">
      <p>
        This brush selection palette offers several ways of
        specifying a Brush. Visual feedback is provided by one or
        two glyphs whose foreground is painted
        with the brush.
      </p>
      <p>
        The text field at the top can be edited to provide a direct specification
        using the <b>Brush</b> parameter notation. Press the <i>Enter</i> button
        to apply the specification.
      </p>
      <p>
        Colours can be selected  by pressing one of the colours buttons directly below
        the text field: these select from the palette of named colours.
      </p>
      <p>
        They can also be specified by pressing hue, saturation, and vibrance (brightness) on
        the rows of buttons below the feedback glyphs.
      </p>
      <p>
        Other Brush properties can be selected from the menus that appear between the
        colours buttons and the feedback glyphs; or by modifying the font descriptor window
        and typing the Enter key.
      </p>
    </div>.enlarged(30))
  }

    lazy val SAMPLE: Glyph = {
      val model = (rectangle(textModel.w, textModel.h)(protoBrush) ||| textModel)
      (model ~~~ rectangle(30+model.w,30+model.h)(transparent)).asGlyph.framed(black)
    }

    lazy val SMALLSAMPLE: Glyph = brushField.enlarged(10).framed(protoBrush) // rectangle(textModel.w, textModel.h)(protoBrush).asGlyph

    /** The comprehensive GUI that has all forms of brush property selection, together with the `Sample` that shows
     * the effects of the current choices.
     */
    lazy val GUI: Glyph = Col(align=Center, bg=lightGrey)(COLOURGUI, ex, PROPERTYGUI, ex, SAMPLE: Glyph, ex, HSVGUI).enlarged(30, bg=sheet.backgroundBrush)

    lazy val NOTEXTGUI: Glyph = Col(align=Center, bg=lightGrey)(colourGrid, ex, PROPERTYGUI, ex, HSVGUI, ex, SMALLSAMPLE).enlarged(30, bg=sheet.backgroundBrush)

    /** A GUI for choosing among named colours */
    lazy val COLOURGUI: Glyph = Col(align=Center, bg=lightGrey)(
      brushField.framed(),
      ex,
      colourGrid
    )

    /** A GUI for choosing properties */
    lazy val PROPERTYGUI: Glyph = Col(align=Center, bg=sheet.backgroundBrush)(
      FixedSize.Row(align=Mid, width=colourGrid.w, bg=lightGrey)(
        brushFieldChooserMenu("Width", "0", "1", "2", "4", "6", "8", "10", "15", "20", "25","30"){
          v =>
            protoBrush.strokeWidth(v.toFloat)
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("Cap", "round", "butt", "square"){
          v =>
            val cap = v match { case "round"=>ROUND; case "butt"=>BUTT; case "square"=>SQUARE}
            protoBrush.cap(cap)
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("Mode", "stroke", "fill"){
          v =>
            val mode = v match { case "fill"=>FILL; case "stroke"=>STROKE }
            protoBrush.mode(mode)
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("Rounded", "()", "10",  "20", "30", "40", "50", "60", "70"){
          case "()" =>
            protoBrush.ComposeEffect.noEffect()
            protoBrush.ComposeEffect.noFilter() // cancel blurring
            brushFeedback()
          case v =>
            protoBrush.ComposeEffect.rounded(v.toFloat)
            protoBrush.ComposeEffect.noFilter() // cancel blurring
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("--", "()", "-", "--", "---", "----"){
          def w = protoBrush.strokeWidth+1
          v => v match {
              case "-" => protoBrush.ComposeEffect.dashed(5,5)
              case "--" => protoBrush.ComposeEffect.dashed(10,10)
              case "---" => protoBrush.ComposeEffect.dashed(15,15)
              case "----" => protoBrush.ComposeEffect.dashed(5*w, 5*w)
              case "()" => protoBrush.ComposeEffect.noEffect()
            }
            protoBrush.ComposeEffect.noFilter() // cancel blurring
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("~~", "()", "~", "~~", "~~~", "~~~~"){
          def w = protoBrush.strokeWidth+1
          v => v match {
              case "~" => protoBrush.ComposeEffect.sliced(2*w, 2)
              case "~~" => protoBrush.ComposeEffect.sliced(2*w, 2*w)
              case "~~~" => protoBrush.ComposeEffect.sliced(3*w, 3*w)
              case "~~~~" => protoBrush.ComposeEffect.sliced(4*w, 3*w)
              case "()" => protoBrush.ComposeEffect.noEffect()
            }
            protoBrush.ComposeEffect.noFilter() // cancel blurring
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("Blur", "()", "1", "2", "4", "6", "8"){
          v => v match {
            case "()" =>
              protoBrush.ComposeEffect.noFilter()
            case v =>
              protoBrush.ComposeEffect.blurred(v.toFloat)
          }
            brushFeedback()
        }(menuSheet), ex,
        brushFieldChooserMenu("α", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"){
          v =>
            protoBrush.alpha(v.toFloat)
            protoBrush.ComposeEffect.noFilter() // cancel blurring
            brushFeedback()
        }(menuSheet),
        sheet.hFill(),
        styled.TextButton("Help"){
          _=> if (helpDialogue.running.isEmpty) {
            helpDialogue.InFront(PROPERTYGUI).start()
          }
        }(menuSheet)
      ),
    )

    /** A gui for choosing brush colours */
    lazy val HSVGUI: Glyph = Col(align=Center, bg=lightGrey)(
      ex,
      hueGrid, ex,
      satGrid, ex,
      briGrid
    )

  /** A `Dialogue` inhabited by the comprehensive `GUI` */
  lazy val Dialogue: styled.windowdialogues.Dialogue[Unit] = styled.windowdialogues.Dialogue.FLASH(GUI, title="Brush Design")

}
