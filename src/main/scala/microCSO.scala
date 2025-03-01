package org.sufrin.microCSO

/**
 * A drastically simplified sublanguage of ThreadCSO using only
 * virtual threads.
 *
 * TODO: pay more attention to the details of || terminations
 *       when a component fails/stops, and of the interruption
 *       of waiting peers in the face of channel closure.
 */

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

abstract class Stop(why: String)  extends Throwable(why)
case object InPortClosed  extends Stop("InPortClosed")
case object OutPortClosed extends Stop("OutPortClosed")

trait OutPort[T] {
  def closeOut(): Unit
  def !(t: T): Unit
}

trait InPort[T] {
  def closeIn(): Unit
  def readBefore(muSec: Long): Option[T]
  def ?(): T
  def ?[V](f: Option[T]=>V): V
}

trait Chan[T] extends OutPort[T] with InPort[T] {}

class SynchronousChannel[T](capacity: Int) extends Chan[T] {
  // TODO: deal with port closing with a waiting peer
  val inOpen,        outOpen       = new AtomicBoolean(true)
  val readerWaiting, writerWaiting = new AtomicBoolean(false)

  def reset(): Unit = {
    inOpen.set(true)
    outOpen.set(true)
    buffer.clear()
    readerWaiting.set(false)
    writerWaiting.set(false)
  }

  def closeIn(): Unit  = {
    inOpen.set(false)
  }

  def closeOut(): Unit = {
    outOpen.set(false)
  }

  val buffer = new LinkedBlockingQueue[T](capacity)

  def readBefore(muSec: Long=0): Option[T] = {
    { if (inOpen.get)
      { val t: T = buffer.poll(muSec, TimeUnit.MICROSECONDS)
        if (t != null) Some(t) else None
      }
      else
        throw InPortClosed
    }
  }

  def !(t: T): Unit =
    if (!outOpen.get) throw OutPortClosed else
    if (buffer.offer(t)) {
      writerWaiting.set(false)
    } else {
      writerWaiting.set(true)
      buffer.put(t)
    }

  def ?(): T = {
    if (!outOpen.get) throw OutPortClosed else
    if (inOpen.get)
       if (buffer.size>0 || outOpen.get) {
          readerWaiting.set(true)
          val t = buffer.take()
          readerWaiting.set(false)
          t
       }
       else
         throw OutPortClosed
    else
         throw InPortClosed
  }


  def ?[V](f: Option[T]=>V): V = {
    if (inOpen.get)
      if (buffer.size>0 || outOpen.get)
        f(Some(buffer.take()))
      else
        f(None)
    else
      f(None)
  }
}

class proc(val name: String="", body: () => Unit)  {
  override def toString: String = s"proc($name)"
  locally { println(s"proc($name)")}
  /** Run this process until it terminates */
  def apply(): Unit  = {
    Thread.ofVirtual.start( () => body() ).join()
  }

  def fork(): Thread = Thread.ofVirtual.start( () => body() )

  def ||(p: proc): proc = proc(s"$name || ${p.name}") {
    val runp = p.fork()
    println(s"$p forked")
    body()
    println(s"$name awaiting $p")
    runp.join()
  }

  /** Coerce a closure to a proc  */
  def ||(body: => Unit): proc = proc.par(this, proc()(body))

}

object proc {
  val procSerial: AtomicLong = new AtomicLong(0)

  def apply(name: String = s"#${procSerial.getAndAdd(1)}")(body: => Unit): proc = new proc(name, () => body)

  def par(main: proc, procs: proc*): proc = procs.fold(main)(_.||(_))

  def repeat(cond: => Boolean)(body: => Unit): Unit = {
    try {
      while (cond) {
        body
      }
    }
    catch {
      case exn: Stop =>
    }
  }

  def attempt(body: => Unit)(alternative: => Unit): Boolean = {
    try {
      body
      true
    }
    catch {
      case exn: Stop =>
        alternative
        false
    }
  }

  def repeatFor[T](iterable: Iterable[T])(body: T => Unit): Unit = {
    val it = iterable.iterator
    while (it.hasNext && attempt{ body(it.next()) }{})  {}
  }
}

object microCSO {
  import proc.{attempt, par, repeat, repeatFor}
  def main(args: Array[String]): Unit = {
    val c = new SynchronousChannel[String](2)
    val p: proc = proc ("p") {
      var going: Boolean = true
      println("p started")
      repeat (going) {
        c ? {
          case Some(s) => println(s)
          case None => going = false
        }
      }
      println("p terminated")
    }
    val q: proc = proc("q") {
      println("q started")
      repeatFor ("the rain in spain falls mainly in the plain . old fruit".split(' '))  {
        s: String =>
        if (s==".") c.closeOut() else attempt {c!s} {}
      }
      println("q terminating")
      c.closeOut()
      println("q terminated")
    }
    println("GO")
    (p||q)()
    println("=======================================")
    c.reset()
    println("=======================================")
    proc.par(p, q)()
    c.reset()
    println("=======================================")
    (par(p, q) || { c!("this is the third branch")})()
  }
}