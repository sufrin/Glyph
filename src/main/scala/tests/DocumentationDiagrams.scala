package org.sufrin.glyph
package tests
import unstyled.static._
import GlyphTypes._

import Brushes.{blueLine, lightGrey, redFrame}
import unstyled.Text


/**
 *  Glyph examples for use in documentation
 */
object DocumentationDiagrams {

  import unstyled.dynamic.OneOf
  import GlyphTransforms.Framed
  import NaturalSize.Col
  import PolygonLibrary._
  import Brushes.{ROUND,SQUARE,BUTT}

  val yellow   = Brush("yellow") color (0xFFffff00) strokeWidth (1.0f) strokeCap(ROUND)
  val green    = Brush("green") color (0xFF00ff00) strokeWidth (5.0f) strokeCap(ROUND)
  val grey     = Brush("grey") color (0xCC000000) strokeWidth (1.0f)
  val black    = Brush("black") color (0xFF000000) strokeWidth (1.0f) antiAlias(false)
  val blue     = Brush("blue") color (0xFF0000ff) strokeWidth (1.0f) strokeCap(ROUND)
  val red      = Brush("red") color (0xFFff0000) strokeWidth (1.0f) strokeCap(ROUND)
  val white    = Brush("white") color (0xFFffffff) strokeWidth (1.0f) strokeCap(ROUND)
  val nothing  = Brush("nothing") color (0)

  object Wide {
    val blue  = Brush("Wide.blue") color (0xFF0000ff) strokeWidth (15.0f) strokeCap(SQUARE)
    val red   = Brush("Wide.red") color (0xFFff0000) strokeWidth (15.0f) strokeCap(SQUARE)
    val green = Brush("Wide.green") color (0xFF00ff00) strokeWidth (15.0f) strokeCap(SQUARE)
  }

  import GlyphTransforms.{Rotated, Scaled}

  val bananas = Label("Bananas", fg=white, bg=blue)
  val oneof: OneOf = OneOf(bg = Wide.red)(
      Rotated(0)(bananas())
    , Rotated(1)(bananas())
    , Rotated(2)(bananas())
    , Rotated(3)(bananas())
  )

  val oneofnbg: OneOf = OneOf()(
      Rotated(0)(bananas())
    , Rotated(1)(bananas())
    , Rotated(2)(bananas())
    , Rotated(3)(bananas())
  )





  var theFace: Typeface = FontManager.default.matchFamilyStyle("Courier", FontStyle.NORMAL)
  var thePointSize: Float = 14.0f
  val theFont:      Font = new Font(theFace, thePointSize)
  def theText(s: String): Text = Text(s, theFont, fg=black)

  val units = 20f
  def a = FilledRect(5*units, 3*units, fg=yellow)
  def b = FilledRect(5*units, 1*units, fg=green)
  def fa = Framed(fg=grey, bg=nothing)(a)
  def fb = Framed(grey, bg=nothing)(b)



  def write(path: String, captioned: Boolean=true)(glyph: Glyph, _caption: String=""): Unit = {
      val caption = if (_caption.isEmpty) glyph.toString else _caption
      def text(line: String): Glyph = theText(s" $line ")
      lazy val label = Col.aligned(0.0f, caption.split("[\n]").toList.map(text))
      val dir = "PNG"
      if (captioned)
        External.renderBitmap(Col(align=Center)(label, Skip(10), glyph), s"${dir}/cap-$path", scale=5.0f)
      External.renderBitmap(glyph, s"${dir}/$path", scale=0.5f)
    }

  def writeSample(path: String)(glyph: Glyph, caption: String): Unit = {
    def text(line: String): Glyph = (theText(s" $line ").copy(black))
    lazy val label = Col.aligned(0.0f, caption.split("[\n]").toList.map(text))
    val dir = "PNG"
    External.renderBitmap(Col(align=Center)(label, Skip(5), Scaled(0.5f)(Label(s"${glyph.diagonal}", fg=black)), Skip(10), glyph), s"${dir}/$path", scale = 5.0f)
  }

  def main(args: Array[String]): Unit = {
    import NaturalSize.{Col, Row}

    def Blob(rad: Scalar, fg: Brush=nothing, bg: Brush=nothing): Glyph = FilledOval(rad, rad, fg, bg)
    def Box(side: Scalar, fg: Brush=nothing, bg: Brush=nothing): Glyph = Rect(side, side, fg, bg)

    val samples = List[(String, Glyph)] (
      ("Rect(150f, 150f, red, blue)",        Rect(150f, 150f, red, blue)),
      ("Rect(150f, 150f, Wide.red, blue)",   Rect(150f, 150f, Wide.red, blue)),
      ("Rect(150f, 150f, Wide.red, nothing))", Rect(150f, 150f, Wide.red, nothing)),
      ("FilledRect(150f, 150f, Wide.red, blue))", FilledRect(150f, 150f, Wide.red, blue)),
      ("FilledOval(150f, 100f, Wide.red, blue))", FilledOval(150f, 100f, Wide.red, blue)),
      ("FilledOval(150f, 100f, Wide.red, nothing))", FilledOval(150f, 100f, Wide.red, nothing)),
      ("Concentric(\n FilledOval(150f, 100f, Wide.red, nothing),\n Blob(10, blue)))",
          Concentric(FilledOval(150f, 100f, Wide.red, nothing), Blob(10, blue))),
      ("Concentric(\n FilledOval(150f, 100f, Wide.red),\n Box(10, blue)))",
        Concentric(FilledOval(150f, 100f, Wide.red), Box(10, blue))),

      ("Framed(fg=Wide.red)(Box(20, blue, green))",
        Framed(fg = Wide.red)(Box(20, blue, green))),
      ("Framed(fg=Wide.red)(Box(20, blue, nothing))",
        Framed(fg = Wide.red)(Box(20, blue))),

      ("Framed(fg=Wide.red)(Box(20, blue, green))",
        Framed(fg = Wide.red)(Box(20, blue, green))),
      ("Framed(fg=Wide.red, bg=yellow)(Box(20, blue, green))",
        Framed(fg = Wide.red, bg=yellow)(Box(20, blue, green))),

    )

    val lineSamples: List[Glyph] = List(
      Polygon(100, 100, fg = blue(width=3))((0, 0), (100, 50), (50, 100), (0, 0)),
      Polygon(100, 100, fg = blue(width=3))((0,0), (20, 0), (20, 20), (40, 20), (40, 0), (60,0), (60, 60), (80, 60), (80, 0), (100,0)),
      FilledPolygon(200, 200, fg = red)((0, 0), (200, 200), (200, 0), (0, 200)),
      FilledPolygon(200, 200, fg = red)((200, 200), (0, 0), (200, 0), (0, 200)),
      FilledPolygon(150, 150, fg = red.copy width 1 cap Brushes.ROUND)((100, 100), (0, 0), (200, 0), (0, 200), (200, 200)),
      FilledPolygon(100, 100, fg = blue(width=3))((0,0), (20, 0), (20, 20), (40, 20), (40, 0), (60,0), (60, 60), (80, 60), (80, 0), (100,0)),
      FilledPolygon(100, 100, fg = blue(width=3))((100,0), (100, 100), (0, 100), (0,0), (20, 0), (20, 20), (40, 20), (40, 0), (60,0), (60, 60), (80, 60), (80, 0), (100,0)),
      Polygon(100, 100, fg = blue(width=3))((0, 0), (20, 0), (20, 20), (40, 20), (40, 0), (60, 0), (60, 60), (80, 60), (80, 0), (100, 0)),
      Polygon(100, 100, fg = blue(width=3))((100, 0), (100, 100), (0, 100), (0, 0), (20, 0), (20, 20), (40, 20), (40, 0), (60, 0), (60, 60), (80, 60), (80, 0), (100, 0)),

    )

    var n=0
    for { (text, glyph) <- samples } {
      n += 1
      writeSample(s"=s$n.png")(glyph, text)
      writeSample(s"=s$n+.png")(glyph, glyph.toString)
    }

    n=0
    for { glyph <- lineSamples } {
      n += 1
      write(s"cap-lineSample$n.png", false)(glyph, " ")
    }

    write("eg1.png")(Col(fa, fb))
    write("eg1a.png")(Row(Col(fa, fb), Col(fa,fb)), "Row(Col(a, b), Col(a,b)")
    write("eg1b.png")(Col(Row(a,b), Row(a,b)), "Col(Row(a,b), Row(a,b)")
    write("eg1c.png")(Row(align=Mid)(a, b), "Row(align=Mid)(a,b)")
    write("eg1d.png")(Row(align=Mid)(Point(Wide.blue(cap=ROUND)), Skip(Wide.blue.strokeWidth), Rect(150f, 100f, Wide.blue(cap=ROUND))),
    """|Row(align=Mid)(Point(Wide.blue(cap=ROUND)),
       |             Skip(Wide.blue.strokeWidth),
       |             Rect(150f, 100f, Wide.blue(cap=ROUND)))
       |""".stripMargin)

    def eg1e = Row(align=Mid)(Point(Wide.blue),
                            Skip(Wide.blue.strokeWidth),
                            Point(Wide.green), Rect(150f, 100f, Wide.blue))

    write("eg1e.png")(eg1e,
      """Row(align=Mid)(Point(Wide.blue),
        |             Skip(Wide.blue.strokeWidth),
        |             Point(Wide.green), Rect(150f, 100f, Wide.blue)
        |""".stripMargin)

    def eg1ex = Row(align=Mid)(Point(Wide.blue(width=2*Wide.blue.strokeWidth, cap=BUTT)),
      Skip(2*Wide.blue.strokeWidth),
      Rect(150f, 100f, Brush("")(color=red.color, width=Wide.blue.strokeWidth, cap=SQUARE)))

    write("eg1ex.png")(eg1ex,
      """Row(align=Mid)(
        |    Point(Wide.blue(width=2*Wide.blue.strokeWidth, cap=BUTT)),
        |    Skip(2*Wide.blue.strokeWidth),
        |    Rect(150f, 100f,
        |         Brush("")(color=red.color, width=Wide.blue.strokeWidth, cap=SQUARE)))
        |""".stripMargin)

    write("eg1f.png")(eg1e.framed(Wide.red), """eg1e.framed(Wide.red)""")

    val eg1g = Concentric(FilledOval(120f, 80f, green), Point(Wide.red)).framed(fg=Wide.blue, bg=Wide.blue).framed(fg=Wide.red)

    write("eg1g.png")(eg1g,
      """Concentric(FilledOval(120f, 80f, green), Point(Wide.red))
        |           .framed(fg=Wide.blue, bg=Wide.blue)
        |           .framed(fg=Wide.red)""".stripMargin)

    write("eg1gskew.png")(eg1g.skewed(0.5f, 0.0f).framed())

    write("eg1gskew2.png")(eg1g.skewed(-0.5f, 0f).framed())

    write("eg1gskew3.png")(eg1g.skewed(-0.5f, -0.5f).framed())

    write("eg1gskew4.png")(eg1g.skewed(0.5f, 0f).enlarged(27, bg=yellow(alpha=0.2f)).framed(redFrame).turned(45f).mirrored(leftRight=true,topBottom = false).framed(fg=redFrame).enlarged(2))

    write("eg1gskew5.png")(eg1g.skewed(0.5f, 0.0f).framed().turned(30f).framed(fg=redFrame))


    Wide.blue color (0xFFFFFF00)
    write("eg1gBlueToYellow.png")(
      Framed(Wide.red)(Framed(Wide.blue)(Concentric(FilledOval(120f, 80f, green), Point(Wide.red)))),
      """Framed(Wide.blue)(Concentric(FilledOval(120, 80) fg green, Point fg Wide.red))""".stripMargin)
    Wide.blue color (0xFF0000FF)


    val egc = eg1e

    write("egc.png")(egc,
      """Row(align=Mid)(Point(Wide.blue),
        |             Skip(Wide.blue.strokeWidth),
        |             Point(Wide.green), Rect(150f, 100f, Wide.blue)
        |""".stripMargin)

    write("egc2.png")(
      Framed(grey)(Row(bg=white)(egc, egc)),
      """Row(bg=white)(egc, egc)""".stripMargin)

    write("egc1.png")(
      Framed(grey)(Row(bg=white)(bg=white)(egc(), egc())),
      """Row(egc(), egc()""".stripMargin)


    write("eg3.png")(
          Col(align=Center)(a, Rotated(1)(b), Row(a, b, a)),
          "Col(align=Center)(a,\n Rotated(1)(b), \n Row.Bottom(a, b, a))"
    )
    write("eg3a.png")(Row(a, b, a), "Row(a, b, a)")
    write("eg3b.png")(Row(align=Top)(a, b, a), "Row(align=Top)(a, b, a)")
    write("eg3c.png")(Row(align=Mid)(a, b, a), "Row(align=Mid)(a, b, a)")
    write("eg3d.png")(Framed(Wide.blue)(Row(align=Mid)(a, b, a)), "Framed(Wide.blue)(\n Row(align=Mid)(a, b, a))")
    write("eg3d.png")(Framed(Wide.blue)(Row(align=Mid)(a, b, a)), "Framed(Wide.blue)(\n Row(align=Mid)(a, b, a))")


    write("oneof.png")(oneof, oneof.toString)
    write("oneofnbg.png")(oneofnbg, oneofnbg.toString)
    oneof.next()
    write("oneofnext.png")(oneof, oneof.toString)


    val Courier = FontFamily("Courier", "normal", 24)
    val CourierItalic = FontFamily("Courier", "italic", 24)
    val Menlo = FontFamily("Menlo", "normal", 20)
    val MenloBold = FontFamily("Menlo", "bold", 16)
    val MenloTiny = FontFamily("Menlo", "normal", 12)

    write("text1.png")(Text("A text", Courier).framed(redFrame), "Text(\"A text\", Courier).framed(redFrame)")
    write("text1b.png")(Text("A text", MenloBold).framed(redFrame), "Text(\"A text\", MenloBold).framed(redFrame)")
    write("text1i.png")(Text("A text", CourierItalic).framed(redFrame), "Text(\"A text\", CourierItalic).framed(redFrame)")
    write("textrow1.png")(
      Row(align=Baseline)(
        Text("As you can see ", CourierItalic),
        Text("baselines coincide ", MenloBold),
        Text("even in tiny fonts ", MenloTiny),
      ), "Row(align=Baseline)(...)")
    write("textrow2.png")(
      Row(align=Top)(
        Text("As you can see ", CourierItalic),
        Text("baselines do not coincide ", MenloBold),
        Text("especially in tiny fonts ", MenloTiny),
      ), "Row(align=Top)(...)")


    {
      import glyphXML.Language._
      implicit val style: StyleSheet = StyleSheet(fontScale=0.6f)
      val declare: Glyph = <ATTRIBUTES key="tag:p" enlarged="20px"/>
      val gxml1: Glyph= <p width="20em" align="left" frame="red/3" rotated="2">Hello world</p>
      val gxml2: Glyph= <p width="20em" align="left" frame="black/3-5-5">Hello world! Is this the very best you can do?</p>
      val gxml3: Glyph =
        <div width="27em" align="justify">
          <p>
            The <b>brain</b> in <i>pain</i> feels with_out ex_ception as if it is going
            down an uninhibitedly con_voluted drain.
          </p>
          <p>
            But when not in pain, it is some_what sensitive to <bi>rain.</bi>
          </p>
          <p textForeground="red">
            Whatever else you do, try to remember that not_with_stand_ing that <b>glyphXML</b> may
            some_what resemble HTML, it is a completely different lang_uage.
          </p>
        </div>
      write("gxml1.png")(gxml1, "gxml1: Glyph")
      write("gxml2.png")(gxml2, "gxml2: Glyph")
      write("gxml3.png")(gxml3, "gxml3: Glyph")
    }

    val down = Variable(false)
    write("dynamic.png")((Shaded.Dynamic(fg=blue, bg=nothing, delta=10f, down=down)(FilledRect(150, 150, red))))
    down.value=true
    write("dynamicdown.png")((Shaded.Dynamic(fg=blue, bg=nothing, delta=10f, down=down)(FilledRect(150, 150, red))))

    write("staticdown.png")((Shaded.Static(delta=10f, down=true)(FilledRect(150, 100, green))),
                          """Shaded.Static(delta=10f, down=true)(FilledRect(150, 100, green))""")

    write("static.png")((Shaded.Static(delta=10f, down=false)(FilledRect(150, 100, green))),
                        "Shaded.Static(delta=10f, down=false)(FilledRect(150, 100, green))")


   //def filledStar7(brush: Brush) = FilledPolygon.diagonal(Vec(250, 250), fg = brush)(star7Path)
    //def openStar7(brush: Brush) = Polygon.diagonal(Vec(250, 250), fg = brush)(star7Path)

    write("filledstar.png", false)(filledStargon(7)(blueLine))
    write("openstar.png", false)(openStargon(7)(blueLine))

    val blueish  = blue(width=blue.strokeWidth*4, pathEffect=PathEffect.makeDiscrete(25.0f, 4.0f, 1))
    val greenish = blueish(color=0xFF009900)
    def wibbly(brush: Brush) = brush().pathEffect(PathEffect.makeDiscrete(4.0f, 5.5f, 18))

    write("row-c.png", true)(Row(align=Mid)(Rect(100f, 50, fg=blue), Rect(50f, 25f, fg=red)))
    write("row-t.png", true)(Row(align=Top)(Rect(100f, 50, fg=blue), Rect(50f, 25f, fg=red)))
    write("row-b.png", true)(Row(align=Bottom)(Rect(100f, 50, fg=blue), Rect(50f, 25f, fg=red)))
    import FixedSize.Space.fill
    write("row-fixed.png", true)(FixedSize.Row(350f, bg=lightGrey)(fill, FilledRect(50f, 25f, fg=red), FilledRect(100f, 25, fg=blue), FilledRect(50f, 25f, fg=red)).framed(black))
    write("row-fixed-j.png", true)(FixedSize.Row(350f, bg=lightGrey)(
      FilledRect(50f, 25f, fg=red), fill,
      FilledRect(100f, 25, fg=blue), fill,
      FilledRect(50f, 25f, fg=red)).framed(black))


    write("col-c.png", true)(Col(align=Center)(Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))
    write("col-l.png", true)(Col(align=Left)(Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))
    write("col-r.png", true)(Col(align=Right)(Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))

    write("conc.png", true)(Concentric(Point(Wide.green), Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))
    write("conc-t.png", true)(Concentric.Top(Point(Wide.green), Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))
    write("conc-r.png", true)(Concentric.Right(Point(Wide.green), Rect(50f, 25f, fg = blue), Rect(150f, 45f, fg = red)))




    write("filledstarw.png", false)(filledStargon(7)(blueish))
    write("openstarw.png", false)(openStargon(7)(blueish))

    write("filledstarw11.png", false)(filledStargon(11)(blueish))
    write("filledstarw5.png", false)(filledStargon(5)(blueish))
    write("filledstarw5blur.png", false)(filledStargon(5)(blueish.blurred(24f)).framed())
    write("filledstarw5blur20.png", false)(filledStargon(5)(blueish.blurred(48f, 20f, 20f)).framed())
    write("filledstarw3.png", false)(filledStargon(3)(blueish))
    write("filledstarw4.png", false)(filledStargon(4)(blueish))
    write("filledstarw6.png", false)(filledStargon(6)(blueish))
    write("filledstarw8.png", false)(filledStargon(8)(blueish))
    write("filledstarw18.png", false)(filledStargon(18)(blueish))
    write("openstarw18.png", false)(openStargon(18)(blue(width=3)))
    write("filledstarw17.png", false)(filledStargon(17)(blueish))
    write("filledstarw7g.png", false)(filledStargon(7)(greenish))

    val wibRed = Brush("wibRed")(color=0x77ff0000, width=16f, pathEffect=PathEffect.makeDiscrete(4.0f, 15.5f, 3))
    val wibRedder = red(width=12f, pathEffect=PathEffect.makeDiscrete(4.0f, 15.5f, 3), cap=BUTT)

    val para=Label("""This is text with a special edging.""")
    write("redframed.png", false)(
      para.enlarged(20).edged(fg = black(width = 2f).sliced(2.5f, 5f)).enlarged(10))

    write("dashframed.png", false)(
      para.enlarged(20).edged(fg = black(width = 2f).dashed(15f, 5f)).enlarged(10))

    write("redpoly.png", false)(
      Polygon(200, 200, fg = red(width = 4f, pathEffect = PathEffect.makeDiscrete(5f, 100f, 15), cap = ROUND)
      )((0, 100), (200, 100)).enlarged(4).framed())

    val gridTables = {
      import styled._
      implicit object Style extends StyleSheet{}
      import Style.{ex,em}
      val data =
        for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
          Label(f"$i.scaled($scale%1.1f)").scaled(scale)

      Col(align=Center)(
        Col(align=Left)(
          Label(".grid(width=3)(data) -- row data as uniform size cells"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, width=3)(data), ex,
          Label(".grid(height=3)(data) -- col data as uniform size cells"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, height = 3)(data), ex, ex, ex,
          Label(".rows(width=3)(data) -- row data in uniform width columns"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, width = 3).rows(data), ex,
          Label(".cols(height=3)(data) -- col data in uniform height rows"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, height = 3)(data), ex, ex, ex,
          Label(".table(width=3)(data) -- row data as minimal width/height cols/rows"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(width=3)(data), ex,
          Label(".table(height=3)(data) -- col data as minimal width/height cols/rows"),
          NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).table(height=3)(data)
        ) scaled 0.8f enlarged (50))
    }

    val gridOrder: Glyph = {
      implicit object Style extends StyleSheet
      import Style.{ex,em}
      val data =
        for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
          Label(f"$i.scaled($scale%1.1f)").scaled(scale)

      Col(align=Center)(
        // NaturalSize.Grid(fg = blue(width = 0), padx = 10, pady = 10).grid(height = 1)(data), ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 1)(data).enlarged(10f), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10, width=1).rows(data).enlarged(10f), ex,
       // NaturalSize.Grid(fg = blue(width = 0), padx = 10, pady = 10).grid(width = 0,  height=0)(Label("XYZZY") :: data.toList)
      )

    }

    val gridCellFit =  {
      import styled._
      implicit object Style extends StyleSheet{}
      import CellFit._
      import Style.{ex,em}
      val data =
        for {scale <- List(0.75f, 1f, 1.5f); i <- List(1, 1000, 1000000)} yield
          Label(f"$i.scaled($scale%1.1f)").scaled(scale)

      def expanded(method: Method): Seq[Glyph] = {
        val lab = Label(s"fitToCell($method)").scaled(0.75f).cellFit(method)
        data.updated(4, lab)
      }

      Col(align=Center)(
        //Label("grid with data(4).fitToCell(...) [Enlarge/ShiftNorth/ShiftWest/ShiftSouth/ShiftEast/Stretch]"),
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(data), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(Stretch)), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftNorth)), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftSouthWest)), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftSouth)), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftSouthEast)), ex, ex,
        NaturalSize.Grid(fg = red(width = 0), padx = 10, pady = 10).grid(width = 3)(expanded(ShiftEast)), ex, ex,
      ) scaled 0.75f enlarged (50)
    }

    write("gridtable.png", false)(gridTables)
    write("gridcellfit.png", false)(gridCellFit)
    write("gridorder.png", false)(gridOrder)


  }
}
