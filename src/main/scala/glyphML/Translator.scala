package org.sufrin.glyph
package glyphML

object Translator {
  import org.sufrin.glyph.glyphML.AbstractSyntax._
  import Context._

  def translate(context: Env)(tree: Tree): Seq[Glyph] = {
    implicit val style: StyleSheet = context.derivedSheet
    def toText(text: String): unstyled.Text = unstyled.Text(text, style.textFont, fg=style.textForegroundBrush, bg=style.textBackgroundBrush)
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
    println(context.derived.toString)
    val sheet=context.derivedSheet
    val rawLines = text.split('\n').toSeq
    val lines = stripCommonIndentation(rawLines)
    NaturalSize.Col(align=Left, bg = sheet.cdataBackgroundBrush)(lines.map(unstyled.Text(_, font=sheet.cdataFont, fg=sheet.cdataForegroundBrush)))
  }

  def emptyText(tree: Tree): Boolean = tree match {
    case Text(text) => text.forall(_.isSpaceChar)
    case Para(texts) => texts.forall(_.forall(_.isSpaceChar))
    case _ => false
  }

  def translateElement(context: Env)(tag: String, attributes: AttributeMap, child: Seq[Tree]): Seq[Glyph] = {
    import Context.TypedAttributeMap
    tag match {
      case "div" | "body" =>
        val derivedContext = context.updated(attributes)
        child.filterNot(emptyText).flatMap(translate(derivedContext))

      case "p" =>
        val derivedContext = context.updated(attributes)

        val words = child.flatMap(translate(derivedContext))
        val hang = derivedContext.attributes.String("hang", "")
        val hangGlyph = if (hang.isEmpty) None else Some(styled.Label(hang)(derivedContext.sheet))

        //Paragraph.fromGlyphs(sheet, targets.map(_.asGlyph), parHang).enlargedBy(0f, sheet.parSkip)
        List(Paragraph.fromGlyphs(derivedContext.sheet, words, hangGlyph).enlargedBy(0f, derivedContext.sheet.parSkip))


      case _ => Nil
    }
  }

  val rootContext = Context.Env(Map.empty, StyleSheet())

  def fromXML(source: scala.xml.Node): Glyph = {
    import glyphXML.PrettyPrint._
    val ast = AbstractSyntax.fromXML(source)
    ast.prettyPrint()
    val tr = translate(rootContext)(ast)
    println(tr)
    NaturalSize.Col(align=Left)(tr)
  }
}

object TestTranslator extends Application {

  val source =
    <div fontfamily="Times New Roman" textforeground="red" cdatabackground="pink" cdataforeground="red">
      <p align="center" hang=" * ">obnoxiously the rain in spain</p>
      <p align="center"  fontFamily="Menlo" fontScale="0.7" textforeground="black">obnoxiously the rain in spain</p>
      <![CDATA[
            this < is
        quoted text >
      ]]>
    </div>

  val GUI: Glyph = Translator.fromXML(source).enlarged(20).framed(Brushes.blackFrame)

  def title: String = "Test Translator"
}