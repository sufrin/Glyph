package org.sufrin.glyph

import styled.MenuButton
import unstyled.reactive.{Enterable, Reaction}

import org.sufrin.glyph.Brush.{BUTT, ROUND, SQUARE}
import org.sufrin.glyph.Brushes.{black, lightGrey, NonBrush}
import org.sufrin.glyph.Colour.HSV
import org.sufrin.glyph.GlyphShape.{FILL, STROKE}
import org.sufrin.glyph.NaturalSize.{transparent, Col, Grid, Row}
import org.sufrin.glyph.styles.decoration.Framed
import org.sufrin.glyph.GlyphTypes.Scalar
import org.sufrin.glyph.styled.windowdialogues.Dialogue
import org.sufrin.logging

/**
 * A glyph by which brushes can be specified interactively.
 */
class BrushChooser(val protoBrush: Brush, val resultBrush: Brush, val onError: NonBrush=>Unit)(implicit sheet: StyleSheet) {
  thisChooser =>
  
  def brushWarning(anchor: Glyph) = {
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

  val hintSheet: StyleSheet = sheet.copy(fontScale = 0.8f)
  val menuSheet: StyleSheet = hintSheet.copy(buttonDecoration = Framed(black(width=1)))

  def setResultBrush(specification: String): Unit = {
    if (specification.nonEmpty) {
      try {
        val brush = Brushes.Parse(specification)
        resultBrush.copyFrom(brush)
        protoBrush.copyFrom(brush)
        showResultBrush()
      }
      catch {
        case nonBrush: Brushes.NonBrush =>
          println(nonBrush)
          onError(nonBrush)
      }
    }
  }

  def setResultBrush(brush: Brush): Unit = {
    resultBrush.copyFrom(brush)
    showResultBrush()
  }

  def showResultBrush(): Unit = {
    brushField.text = resultBrush.toString
  }

  lazy val brushField = styled.TextField(size=50, onEnter=setResultBrush(_), initialText=resultBrush.toString)(hintSheet)

  def HintedUnstyledButton(label: String, hint: String="", fg: Brush, bg: Brush)(action: Reaction): Glyph = {
    val button = unstyled.reactive.FramedButton(label, fg, bg)(action)
    if (hint.nonEmpty)  HintManager(button.asInstanceOf[Enterable], 5, ()=>hint)(hintSheet)
    button
  }


  def brushFieldChooserMenu(fieldname: String, choices: String*)(choose: String => Unit)(implicit sheet: StyleSheet): Glyph = {
    val buttons = choices.map {
      name => MenuButton(name){ _ => choose(name) }
    }
    val menu = styled.overlaydialogues.Dialogue.Menu(fieldname, nested=false, buttons)
    menu
  }


    import sheet.{em,ex}
    import GlyphShape._

    def brushFeedback(): Unit = setResultBrush(protoBrush.copy())
    def rectangle(w: Scalar, h: Scalar)(brush: Brush): GlyphShape = {
      val path=new PathShape(brush, false)
      path.moveTo(0, 0)
      path.lineTo(w, 0)
      path.lineTo(w, h)
      path.lineTo(0, h)
      path.closePath
      path
    }
    val colButs = for { (name, col) <- Brushes.namedColours  } yield
      HintedUnstyledButton("  ", name, fg=transparent, bg=Brushes(name)){
        _ =>
          protoBrush.setColor(col)
          brushFeedback()
      }
    val hueButs = for { i <- 0 until 24  } yield {
      HintedUnstyledButton("  ", s"${i*15}°", fg=transparent, bg=Brushes(s"hsv(${i*15}).fill")){
        _ =>
          val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
          protoBrush.setColor(HSV(i*15, s, v).argb)
          protoBrush.ComposeEffect.noFilter()
          brushFeedback()
      }
    }
    val satButs =
      for { i<-0 to 10 } yield  HintedUnstyledButton(" ", f"Sat ${i*0.1}%1.1f", fg=transparent, bg=Brushes(s"hsv(0, ${i*0.1}, 1).fill")){
        _ =>
          val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
          protoBrush.setColor(HSV(h, i*0.1, v).argb)
          protoBrush.ComposeEffect.noFilter()
          brushFeedback()
      }
    val briButs =
      for { i<-0 to 10 } yield  HintedUnstyledButton(" ", f"Vib ${i*0.1}%1.1f", fg=transparent, bg=Brushes(s"hsv(0, 1, ${i*0.1}).fill")){
        _ =>
          val HSV(h,s,v) = Colour.intToRGB(protoBrush.color).hsv
          protoBrush.setColor(HSV(h, s, i*0.1).argb)
          protoBrush.ComposeEffect.noFilter()
          brushFeedback()
      }

    val colourGrid = Grid(fg=black).table(width=12)(colButs.toList)
    val hueGrid = Grid(fg=black).table(width=12)(hueButs.toList)
    val satGrid = Grid(fg=black).table(width=11)(satButs.toList)
    val briGrid = Grid(fg=black).table(width=11)(briButs.toList)
    val textModel = unstyled.Text(" Textual ", sheet.labelFont.makeWithSize(36), protoBrush)

  lazy val helpDialogue: Dialogue[Unit] = {
    import glyphXML.Language._
    styled.windowdialogues.Dialogue.FLASH(<div width="50em" align="justify" parskip="1.5ex">
      <p>
        This button selection palette offers several ways of
        specifying a Brush. Visual feedback is provided by a
        showing couple of glyphs whose foreground is specified
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
        colours buttons and the feedback glyphs.
      </p>
    </div>.enlarged(30)).InFront(COLOURGUI)
  }

    lazy val sample =
      asGlyph((rectangle(textModel.w, textModel.h)(protoBrush) ||| textModel) ~~~ rectangle(30+2*textModel.w,30+textModel.h)(transparent)).framed(black)

    lazy val GUI = Col(align=Center, bg=lightGrey)(COLOURGUI, ex, sample, ex, HUEGUI).enlarged(30)

    lazy val COLOURGUI = Col(align=Center, bg=lightGrey)(
      brushField.framed(), ex,
      colourGrid, ex,
      FixedSize.Row(align=Mid, width=colourGrid.w)(
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
        styled.TextButton("Help"){ _=> helpDialogue.show() }(menuSheet)
      ),
    )

    lazy val HUEGUI = Col(align=Center, bg=lightGrey)(
      ex,
      hueGrid, ex,
      satGrid, ex,
      briGrid, ex,
      ex,ex
    )


  lazy val Dialogue: styled.windowdialogues.Dialogue[Unit] = styled.windowdialogues.Dialogue.FLASH(GUI, title="Brush Palette")
}
