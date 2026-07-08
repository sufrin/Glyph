package org.sufrin.utility
import java.nio.file.{Files, Path}
import scala.Console.out

/**
* A `Cursor` for text sequences that also keeps track of the coordinates of
 * `current`.
*/
class SourceTextCursor(iterator: Iterator[Char]) extends Cursor[Char] {
  private var _lines = 1
  private var _chars = 0
  private var _lastChar: Char = '\u0000' // NUL

  var prompting = false
  private var makePrompt: () => String = ()=>""

  /**
   *  Use the expression `prompt` to generate (a new) prompt string
   *  If it yields "" the first tme it is invoked, then stop prompting.
   */
  def withPrompt(prompt: => String): this.type = {
    makePrompt = () => prompt
    prompting  = true
    this
  }

  /** Invoked whenever `next()` is invoked after the last character read was `\n` */
  @inline def prompt(): Unit = { out.flush(); print(makePrompt()); out.flush() }

  def withStartLocation(atLine: Int, atCol: Int = 0): this.type = {
    _lines = atLine+1
    _chars = atCol
    this
  }


  private var _path: String = ""

  def withPath(path: String): this.type = {
    _path = path
    this
  }

  def path: String = _path



  /** `(lines, chars)` are the coordinates of `current`  */
  def chars: Int = _chars
  /** `(lines, chars)` are the coordinates of `current`  */
  def lines: Int = _lines

  /**
   * Returns the next character from the iterator; maintaining `chars`, `lines` as
   * the coordinates of the current position in the text, and prompting when
   * that is appropriate.
   */
  @inline private def getNext(): Char = {
    if (prompting && (_lastChar=='\n' || _lastChar=='\u0000') ) prompt()
    val n = iterator.next()
    _lastChar match {
      case 0 =>
      case '\n' =>
        _lines+=1
        _chars= 0
      case _ =>
        _chars += 1
    }

    _lastChar=n
    n
  }

  def hasCurrent: Boolean =
    buffer.isDefined || (iterator.hasNext && { buffer=Some(getNext()); true })
  private var buffer: Option[Char] = None
  def current: Char = buffer.get
  def current_=(element: Char): Unit = buffer=Some(element)
  def next(): Unit = {
    buffer = None
    hasCurrent
    ()
  }
}

object SourceTextCursor {

  def apply(iterator: Iterator[Char]): SourceTextCursor = new SourceTextCursor(iterator)

  def apply(iterable: Iterable[Char]): SourceTextCursor = new SourceTextCursor(iterable.iterator)

  def apply(path: Path): SourceTextCursor = {
    if (path.toString == "/dev/console")
       console
    else
       new SourceTextCursor(Files.readString(path).iterator).withPath(path.toString)
  }

    def console: SourceTextCursor = {
      val reader = new java.io.InputStreamReader(System.in)
      //Option(System.console()) match {
      // case Some(console) => console.reader()
      //  case None =>
      //}
      val it = new Iterator[Char] {
        def hasNext: Boolean =  true
        def next(): Char = {
          reader.read() match {
            case -1    => '\u0004'
            case other => other.toChar
          }
        }
      }
      new SourceTextCursor(it)
  }
}

/*
object SourceTextCursorTest extends App {
  val c = SourceTextCursor("the\nrain\nin")
  while (c.hasCurrent) {
    println(s"(${c.lines}.${c.chars} (${c.current}")
    c.next()
  }
}
*/
