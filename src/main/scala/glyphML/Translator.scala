package org.sufrin.glyph
package glyphML

import glyphXML.PrettyPrint

object Translator {
  import org.sufrin.glyph.glyphML.AbstractSyntax._
  import Context._

  def translate(context: Env)(tree: Tree): Seq[Glyph] = {
    implicit val style: StyleSheet = context.sheet
    def interWordSpace() = style.hFill(1, 0.5f)

    def toText(text: String): Glyph = {
      if (text.isEmpty || text(0).isWhitespace)
        interWordSpace()
      else
        unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush)
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

  private def leadingSpaces(s: String): Int = {
    var n = 0
    while (n<s.length && s(n)==' ') n+=1
    n
  }

  private val indentationOrdering: Ordering[String] = new Ordering[String] {
    def compare(x: String, y: String): Int = leadingSpaces(x)-leadingSpaces(y)
  }

  protected def stripCommonIndentation(lines: Seq[String]): Seq[String] = {
    val lines$ = lines.dropWhile(_.isBlank)
    val prefix = leadingSpaces(lines$.filterNot(_.isBlank).min(indentationOrdering))
    def stripPrefix(line: String): String = if (line.length<prefix) line else line.substring(prefix)
    if (lines$.nonEmpty && lines$.last.isBlank)
      lines$.init.map(stripPrefix)
    else
      lines$.map(stripPrefix)
  }


  def translateQuoted(context: Env)(text: String): Glyph = {
    val sheet=context.sheet
    val rawLines = text.split('\n').toSeq
    val lines = stripCommonIndentation(rawLines)
    NaturalSize.Col(align=Left, bg = sheet.cdataBackgroundBrush)(lines.map(unstyled.Text(_, font=sheet.cdataFont, fg=sheet.cdataForegroundBrush)))
  }

  def emptyText(tree: Tree): Boolean = tree match {
    case Text(text) => text.forall(_.isWhitespace)
    case Para(texts) => texts.forall(_.forall(_.isWhitespace))
    case _ => false
  }

  def translateElement(context: Env)(tag: String, localAttributes: AttributeMap, child: Seq[Tree]): Seq[Glyph] = {
    import Context.TypedAttributeMap
    tag match {
      case "div" | "body" =>
        val derivedContext = context.updated(localAttributes)
        child.filterNot(emptyText).flatMap(translate(derivedContext))

      case "p" =>
        val derivedContext = context.updated(localAttributes)
        val words = child.flatMap(translate(derivedContext))
        val hang = derivedContext.attributes.String("hang", "")
        val hangGlyph = if (hang.isEmpty) None else Some(styled.Label(hang)(derivedContext.sheet))

        List(Paragraph.fromGlyphs(derivedContext.sheet, words, hangGlyph).enlargedBy(0f, derivedContext.sheet.parSkip).framed())
      //Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)

      case "span" =>
        val derivedContext = context.updated(localAttributes)
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
          child.filterNot(emptyText).foreach{ tree => println(PrettyPrint.prettyPrint(tree)) }
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

  val source =
    <div fontfamily="Menlo" width="70em" textforeground="red" cdatabackground="pink" cdataforeground="red">
      <p align="justify" hang=" * ">Menlo the rain in spain <span textforeground="green">falls mainly</span> in the plain, and may go further.</p>
      <p align="justify"  fontFamily="Courier" fontScale="0.7" textforeground="black">Courier the rain in spain may well spill over two lines if I am not mistaken.</p>
      <p align="center"  fontFamily="Arial" fontScale="0.7" textforeground="black">Arial the rain in spain may well spill over two lines if I am not mistaken.</p>
      <debug local="t" tree="t"  caption="DEBUG1">
      <![CDATA[
            this < is
        quoted text >
      ]]>
      </debug>
    </div>

  val GUI: Glyph = Translator.fromXML(source).enlarged(20).framed(Brushes.blackFrame)

  def title: String = "Test Translator"
}