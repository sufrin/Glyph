package org.sufrin.glyph
package glyphML

import glyphXML.PrettyPrint

import org.sufrin.glyph.unstyled.static.BreakableGlyph

object Translator {
  import org.sufrin.glyph.glyphML.AbstractSyntax._
  import Context._
  implicit class SemanticStyleSheet(val style: StyleSheet) extends AnyVal {
    def makeText(text: String): Glyph = unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush)
  }

  object HYPHENATION {
    // TODO: memoization, automation, ...
    def forText(text: String, style: StyleSheet): Option[Glyph] = {
      val discretionaryWordBreak = style.discretionaryWordBreak

      @inline def discretionary(text: String): Option[Glyph] =
        if (text.contains(discretionaryWordBreak)) {
          val parts = text.split(discretionaryWordBreak)
          val glyphs = parts.toSeq.map(style.makeText(_))
          val hyphen = style.makeText("-")
          Some(new BreakableGlyph(hyphen, glyphs))
        }
        else None

      hyphenation.get(text) match {
        case Some(parts) =>
          println(s"$text->${parts.toList}")
          val glyphs = parts.toSeq.map(style.makeText(_))
          val hyphen = style.makeText("-")
          Some(new BreakableGlyph(hyphen, glyphs))

        case None =>
          discretionary(text)
      }


    }

    val hyphenation: collection.mutable.Map[String, Array[String]] = collection.mutable.Map[String, Array[String]]()

    def apply(word: String)(implicit punct: String="-"): Unit =
      hyphenation.update(word.replaceAll(punct,""), word.split(punct))

  }

  /**
   *  Translate this tree (and its descendants, if any) to `Glyph`s
   *  derived from it in the given environment.
   */
  def translate(context: Env)(tree: Tree): Seq[Glyph] = {
    implicit val style: StyleSheet = context.sheet

    def interWordSpace() = style.textWordFill(0.5f)

    def toText(text: String): Glyph = {
      if (text.isEmpty || text(0).isWhitespace)
        interWordSpace()
      else {
        HYPHENATION.forText(text, style) match {
          case None             => style.makeText(text)
          case Some(hyphenated) => hyphenated
        }
      }
    }

    tree match {
      case Element(tag: String, attributes: AttributeMap, child: Seq[Tree]) => translateElement(context)(tag, attributes, child)

      case Text(text: String)       => List(toText(text))
      case Para(texts: Seq[String]) => texts.map(toText(_))

      case Quoted(text: String)     => List(translateQuoted(context)(text))

      case Entity(name: String)     => Nil

      case Comment(target: String, text: String) =>
        (target, text) match {
          case ("", "SHEET") =>
            List(toText(style.toString))
          case _ => Nil
        }
    }
  }

  object PCDATA {
    private def leadingSpaces(s: String): Int = {
      var n = 0
      while (n < s.length && s(n) == ' ') n += 1
      n
    }

    private val indentationOrdering: Ordering[String] = new Ordering[String] {
      def compare(x: String, y: String): Int = leadingSpaces(x) - leadingSpaces(y)
    }

    def stripCommonIndentation(lines: Seq[String]): Seq[String] = {
      val lines$ = lines.dropWhile(_.isBlank)
      val prefix = leadingSpaces(lines$.filterNot(_.isBlank).min(indentationOrdering))
      @inline
      def stripPrefix(line: String): String = if (line.length < prefix) line else line.substring(prefix)

      if (lines$.nonEmpty && lines$.last.isBlank)
        lines$.init.map(stripPrefix)
      else
        lines$.map(stripPrefix)
    }
  }


  def translateQuoted(context: Env)(text: String): Glyph = {
    val sheet=context.sheet
    val rawLines = text.split('\n').toSeq
    val lines = PCDATA.stripCommonIndentation(rawLines)
    NaturalSize.Col(align=Left, bg = sheet.cdataBackgroundBrush)(lines.map(unstyled.Text(_, font=sheet.cdataFont, fg=sheet.cdataForegroundBrush)))
  }

  def isEmptyText(tree: Tree): Boolean = tree match {
    case Text(text) => text.forall(_.isWhitespace)
    case Para(texts) => texts.forall(_.forall(_.isWhitespace))
    case _ => false
  }

  def translateElement(context: Env)(tag: String, localAttributes: AttributeMap, child: Seq[Tree]): Seq[Glyph] = {
    import Context.TypedAttributeMap
    tag match {
      case "div" | "body" =>
        val derivedContext = context.updated(localAttributes)
        child.filterNot(isEmptyText).flatMap(translate(derivedContext))

      case "p" =>
        val derivedContext = context.updated(localAttributes)
        val words = child.flatMap(translate(derivedContext))
        val hang = derivedContext.attributes.String("hang", "")
        val hangGlyph = if (hang.isEmpty) None else Some(styled.Label(hang)(derivedContext.sheet))
        val theGlyph = Paragraph.fromGlyphs(derivedContext.sheet, words, hangGlyph).enlargedBy(0f, derivedContext.sheet.parSkip)
        val frame = derivedContext.attributes.Brush("frameparagraphs", Brushes.transparent)
        List(if (frame.getAlpha==0) theGlyph else theGlyph.framed(frame))
      //Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)

      case "span" =>
        val derivedContext = context.updated(localAttributes)
        child.flatMap(translate(derivedContext))

      case "i" =>
        val style = Map("textstyle"->"italic")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "b" =>
        val style = Map("textstyle"->"bold")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "tt" =>
        val style = Map("fontfamily"->"courier")
        val derivedContext = context.updated(style supersede localAttributes)
        child.flatMap(translate(derivedContext))

      case "debug" =>
        val derivedContext = context.updated(localAttributes.without("tree", "env", "caption"))
        val tree = localAttributes.Bool("tree", false)
        val env = localAttributes.Bool("env", false)
        val att = localAttributes.Bool("local", false)
        val title = localAttributes.String("caption", "debug")
        if (tree || env || att) println(title)
        if (env) println(PrettyPrint.prettyPrint(derivedContext)) else if (att) println(PrettyPrint.prettyPrint(derivedContext.attributes)) else {}
        if (tree && child.nonEmpty)
          child.filterNot(isEmptyText).foreach{ tree => println(PrettyPrint.prettyPrint(tree)) }
        child.flatMap(translate(derivedContext))

      case _ => Nil
    }
  }

  val rootContext = Context.Env(Map.empty, StyleSheet())

  def fromXML(source: scala.xml.Node): Glyph = {
    import glyphXML.PrettyPrint._
    val ast = AbstractSyntax.fromXML(source)
    //ast.prettyPrint()
    val tr = translate(rootContext)(ast)
    NaturalSize.Col(align=Left)(tr)
  }
}

object TestTranslator extends Application {

  Translator.HYPHENATION("flocci/nauci/nihil/ipil/ifica/tion")("/")
  Translator.HYPHENATION("anti_dis_estab_lish_men_t_arian_ism")("_")
  Translator.HYPHENATION("averywidewordwithaninfeasible/breakpoint")("/")

  val source =
    <div fontfamily="Menlo" width="40em" textforeground="red" cdatabackground="pink" cdataforeground="red" frameparagraphs="red.0.dashed(3,3)">
      <p align="justify" hang=" * ">Menlo the rain in spain <span textforeground="green">falls mainly</span> in the plain, and may go further.</p>
      <p align="justify"  fontFamily="Courier" fontscale="0.9" textforeground="black">Courier the <i>italic font</i> rain in spain may well spill over two lines if I am not mistaken.</p>
      <p align="center"  fontFamily="Arial" fontScale="0.5" textforeground="black">Arial the rain in spain may well spill over two lines if I am not mistaken.</p>
      <debug xlocal="t" xtree="t"  xcaption="DEBUG1">
      <!--![CDATA[
            this < is
        quoted text >
      ]]-->
      </debug>
      <p align="justify"  width="400px" fontFamily="Arial" fontscale="1" textforeground="black">
        This is going to be a long, hyphen_at_able antidisestablishmentarianism tract_ified text <tt textbackground="pink" fontscale="1.1">tele_type font</tt>
        that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed.
      </p>

      <p align="justify"  width="300px" fontFamily="Menlo" fontscale="0.8" textforeground="black">
        <debug tree="t">This is going to be a long, hyphen_at_able "antidisestablishmentarianism" tract_ified text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
        </debug>
      </p>
      <p align="justify"  width="300px" fontFamily="Arial" fontscale="0.8" textforeground="black">
        This is going to be a long, goddam, floccinaucinihilipilification text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
        by the fans of antidisestablishmentarianism or floccinaucinihilipilification at the end of a sen_tence.
      </p>
      <p align="justify"  width="20em" fontFamily="Arial" fontscale="0.8" textforeground="black">
        This is going to be a long, goddam, floccinaucinihilipilification text that spills ov_er a nar_row mar_gin un_less I am mis_tak_enly in_formed
        by the fans of antidisestablishmentarianism or floccinaucinihilipilification or averywidewordwithaninfeasiblebreakpoint at the end of a sen_tence.
      </p>
    </div>

  val GUI: Glyph = Translator.fromXML(source).enlarged(20).framed(Brushes.blackFrame)

  def title: String = "Test Translator"
}