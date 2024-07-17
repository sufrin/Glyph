package org.sufrin.microCSO

/**
 * A drastically simplified sublanguage of ThreadCSO using only
 * virtual threads.
 */

import org.sufrin.logging._
import org.sufrin.microCSO.termination._

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}



trait OutPort[T] {
  def closeOut(): Unit
  def !(t: T): Unit
}

trait InPort[T] {
  def closeIn(): Unit
  def readBefore(muSec: Long): Option[T]
  def ?(t: Unit): T
  def ?[V](f: T=>V): V = f(this.?(()))
}

trait Chan[T] extends OutPort[T] with InPort[T] {}

class SyncChan[T](name: String, capacity: Int) extends Chan[T] {
  import termination._
  override val toString: String = s"$name($capacity)"

  val inOpen, outOpen       = new AtomicBoolean(true)
  /** When non-null this is a peer waiting to write/read */
  val waitingWriter, waitingReader = new AtomicReference[Thread]

  def reset(): Unit = {
    inOpen.set(true)
    outOpen.set(true)
    buffer.clear()
    waitingWriter.set(null)
  }

  /** close the input port, and interrupt any waiting writer */
  def closeIn(): Unit  = {
    inOpen.set(false)
    Default.finest(s"$this CloseIn()")
    val peer = waitingWriter.getAndSet(null)
    if (peer ne null) peer.interrupt()
  }

  /** close the output port, and tell readers (once)*/
  def closeOut(): Unit = {
    Default.finest(s"$this CloseOut()")
    outOpen.set(false)
    val peer = waitingReader.getAndSet(null)
    if (peer ne null) peer.interrupt()
  }

  val buffer = new LinkedBlockingQueue[T](capacity)

  def readBefore(muSec: Long=0): Option[T] =
      if (inOpen.get) {
        val msg = buffer.poll(muSec, TimeUnit.MICROSECONDS)
        if (msg==null)
           None
        else
           Some(msg)
      }
      else
        throw new Closed(name)

  def !(t: T): Unit = {
      if (!(inOpen.get && outOpen.get)) throw new Closed(name) else
      if (buffer.offer(t)) {
        waitingWriter.set(null)
      } else {
        waitingWriter.set(Thread.currentThread())
        try { buffer.put(t) } catch { case _: InterruptedException => }// perhaps interrupted
        waitingWriter.set(null)
      }
    }

  def ?(t: Unit): T =
      if (inOpen.get) {
        if (!outOpen.get && buffer.isEmpty) {
          throw new Closed(name)
        } else {
          val t: T = buffer.poll()
          try {
            if (t == null) {
              waitingReader.set(Thread.currentThread())
              val w: T = buffer.take()
              waitingReader.set(null)
              w
            } else {
              waitingReader.set(null)
              t
            }
          }
          catch {
            case _: InterruptedException =>
              throw new Closed(name)
          }
        }
      } else
        throw new Closed(name)
}

object SyncChan extends serialNamer {
  val namePrefix: String = "SyncChan"
  def apply[T](capacity: Int): SyncChan[T] = new SyncChan[T](nextName(), capacity)
  def apply[T](name: String, capacity: Int): SyncChan[T] = new SyncChan[T](name, capacity)
}


/**
 *  A `process` is a prototype for an entity that can be applied
 *  or `forked`.  In the former case it runs in the JVM thread from which it
 *  was applied; in the latter case it runs in a newly-acquired
 *  JVM thread.
 *
 *  It can also be composed in parallel with another `process`.
 */
trait process extends (()=>Unit) {
   override def toString: String = name

   /** Run in the current thread  */
   def apply(): Unit
  /**
   *  Run in a newly-acquired thread; yielding a handle from which the
   *  thread can be interrupted or awaited.
   */
  def fork():  ForkHandle

  /** Name of the process */
  def name: String

  /** parallel composition of `this: process` with `that: process` */
  def ||(that: process): process
 }

import java.util.concurrent.{CountDownLatch => Latch}

class ForkHandle(val name: String, val body: ()=>Unit, val latch: Latch) extends Runnable {
  private val logging: Boolean = true
  override def toString: String =
    s"ForkHandle($name, count=${if(latch==null) "<>" else latch.getCount.toString}){ \n|  status=$status\n|  thread=${thread.getName} }"
  var thread: Thread = null
  var status: Status = UnStarted

  def interrupt(): Unit = if (thread ne null) thread.interrupt()

  def join(): Unit = latch.await()

  override def run(): Unit = {
    var prevName: String = ""
    try {
      thread = Thread.currentThread
      prevName = thread.getName
      thread.setName(name)
      status = Running
      body()
      status = Terminated
    } catch {
      case thrown: Anticipated =>
        if (logging) Default.finest(s"$this.body() threw $thrown")
        status = thrown
      case thrown: Throwable   =>
        status = new unAnticipated(thrown)
        if (logging) Default.finest(s"$this.body() threw $thrown")
    } finally {
      thread.setName(prevName)
    }
    if (logging) Default.finest(s"($this).run() => $status")
    if (latch!=null) latch.countDown()
  }

  /** Await termination of this running fork then yield `(true, status)` */
  def terminationStatus(): (Boolean, Status) = {
    if (latch!=null) latch.await()
    (true, status)
  }

  /** Await termination of this running fork for no more than `muSec` microseconds,
   *  then yield `(terminated, status)`, where `terminated`
   *  is true iff the fork terminated by the deadline.
   */
  def terminationStatus(muSec: Long): (Boolean, Status) = {
    val terminated =
        if (latch==null) true else latch.await(muSec, TimeUnit.MICROSECONDS)
    (terminated, status)
  }

  /** Acquire a new JVM thread, and evaluate `body()` in it */
  def start(): Unit = {
    thread = Thread.ofVirtual.start( () => run() )
  }
}

/**
 *  Constructor for a simple `process`
 */
class proc(val name: String="", val body: ()=>Unit)  extends process { thisProc =>
  import termination._
  private val logging: Boolean = true

  def apply(): Unit = body()

  def fork(): ForkHandle = {
    val handle = new ForkHandle(name, apply, new Latch(1))
    handle.start()
    handle
  }

  /**
   *  A `proc` that runs `this`  and `that` in parallel until they have both
   *  terminated, then propagates an appropriately informative termination status.
   *
   *  @see Status.|||
   */
   def ||(that: process): process = new par(List(this, that))


  /** Syntactic sugar: coerce a closure to a proc  */
  def ||(body: => Unit): process = new par(List(this, proc { body }))

}

class par(components: Seq[process]) extends process {
  private val logging: Boolean  = true
  def name: String = components.map(_.name).mkString("(", "||", ")")
  def ||(that: process): process = new par(List(this, that))

  def apply(): Unit = {
    val latch       = new Latch(components.length-1)
    val peerHandles = components.tail map {
      proc => new ForkHandle(proc.name, proc, latch)
    }
    val firstHandle = {
      val proc = components.head
      new ForkHandle(proc.name, proc, null)
    }

    for { handle <- peerHandles } { handle.start() }
    // run the first component in the current thread
    firstHandle.run()
    // await the peers
    latch.await()
    // calculate and propagate the status
    var status = firstHandle.status
    for { peer <- peerHandles } {
      status = termination.|||(status, peer.status)
    }
    status.propagate()
  }

  def fork(): ForkHandle = {
    val handle = new ForkHandle(name, apply, new Latch(1))
    handle.start()
    handle
  }
}

/**
 * Implementation of the algebra of termination that is used to support
 * coherent termination of `||` constructs. `P||Q` constructs a process
 * that terminates when both components have terminated. Its termination
 * status is the "more informative" of the statuses of its components, in
 * the ordering `unAnticipated > Anticipated > Terminated`. This
 * supports the "outward" propagation of non-`Terminated` statuses.
 * // TODO: It may, at some point, be useful to preserve the detail of
 *          the termination statuses; in which case another constructor
 *          can be added.
 */
object termination {
  private val logging: Boolean = true
  trait Status {
    /** one of `this,r` that is no less informative than the other*/
    def |||(r: Status):Status = termination.|||(this, r)
    def propagate(): Unit
  }

  lazy val Stopped: Anticipated = new Anticipated("stop")

  /**
   *  Pronounce this "no less informative"
   *  @return one of `l,r` that is no less informative than the other
   */
  def |||(l: Status, r: Status): Status = {
    (l, r) match {
      case (_:unAnticipated, _)               => l
      case (_,               _:unAnticipated) => r
      case (_:Anticipated,   _)               => l
      case (_,               _:Anticipated)   => r
      case (Terminated,      r)               => r
      case (l,               Terminated)      => l
    }
  }

  case class unAnticipated(throwable: Throwable) extends Status {
    override def toString: String = s"unAnticipated($throwable)"
    def propagate(): Unit = {
      if (logging) Default.finest(s"$this.propagate()")
      throw throwable
    }
  }

  class Closed(name: String) extends Anticipated(s"Closed($name)") {
    override def toString: String = s"Closed($name)"
    override def propagate(): Unit = {
      if (logging) Default.finest(s"$this.propagate()")
      throw this
    }
  }

  class Anticipated(why: =>String)  extends Throwable with Status {
    override def toString: String = s"$why"
    override def propagate(): Unit = {
      if (logging) Default.finest(s"$this.propagate()")
      throw this
    }
  }

  case object Terminated extends Status {
    override def propagate(): Unit =
      if (logging) Default.finest(s"$this.propagate()")
      ()
  }

  case object Running extends Status {
    override def propagate(): Unit = throw new Exception("Running must not propagate")
  }

  case object UnStarted extends Status {
    override def propagate(): Unit = throw new Exception("Unstarted must not propagate")
  }

}

trait serialNamer {
  val serial: AtomicLong = new AtomicLong(0)
  val namePrefix: String
  def nextName(): String = s"$namePrefix#${serial.getAndAdd(1)}"
}

object proc extends serialNamer {
  private val logging: Boolean = true

  import termination._
  val namePrefix = "proc"

  def logStatus(status: Status): Unit = {
    if (logging) Default.finest(s"(${Thread.currentThread().getName})() ==> $status")
    if (logging) {
      status match {
        case unAnticipated(throwable: Throwable) =>
          throwable.printStackTrace()
        case Terminated =>
        case throwable: Anticipated =>
          throwable.printStackTrace()
      }
    }
  }

  def apply(name: String)(body: => Unit): proc = new proc(name, ()=>body)

  def apply(body: => Unit): proc = new proc(nextName(), ()=>body)

  def stop: Unit = throw termination.Stopped
  def fail(why: String): Unit = throw new Error(s"fail($why) from ${Thread.currentThread.getName}")

  def ||(procs: Seq[proc]): process = new par(procs)


  /** Iterate `body` while the evaluation of `guard` yields `true`. If an
   * exception ''ex'' is thrown, then stop the iteration; and then unless ''ex'' is
   * a `Anticipated` re-throw ''ex''.
   */
  def repeat(guard: => Boolean)(body: => Unit): Unit = {
    var go = guard
    while (go)
      try {
        body; go = guard
      }
      catch {
        case a: Anticipated => if (logging) Default.finest(s"repeat => $a"); go = false
        case t: Throwable   => throw t
      }
  }

  def repeatedly(body: => Unit): Unit = {
    var go = true
    while (go)
      try {
        body
      }
      catch {
        case a: Anticipated => if (logging) Default.finest(s"repeatedly => $a"); go = false
        case t: Throwable => throw t
      }
  }

  /** Evaluate `body` and return its value unless an exception ''ex'' is thrown.
   * If ''ex'' is a `Anticipated` then evaluate and return the value of
   * `alternative`, otherwise re-throw ''ex''.
   */
  def attempt[T](body: => T)(alternative: => T): T = {
    try {
      body
    }
    catch {
      case _: Anticipated => alternative
      case t: Throwable   => throw t
    }
  }


  /** `repeatFor (it: Iterable[T]) { bv => body }` applies the function `{ bv =>
   * body }` to each of the elements of an iterator formed from the iterable.
   * If an exception ''ex'' is thrown, then stop the iteration; then unless
   * ''ex'' is a `Anticipated` re-throw ''ex''.
   */
  def repeatFor[T](iterable: Iterable[T])(body: T => Unit): Unit =
    attempt {
      for (t <- iterable) body(t)
    } {}
}

object portTools {
  private val logging: Boolean = true
  import proc._


  def zip[A,B](as: InPort[A], bs: InPort[B])(abs: OutPort[(A,B)]): proc = proc(s"zip($as,$bs)($abs)") {
    var a = null.asInstanceOf[A]
    var b = null.asInstanceOf[B]
    val read = (proc (s"$as?()") { as ? { x=>a=x } }) || (proc (s"$bs?()") { bs ? { y => b=y} })
    repeatedly {
      read()
      if (logging) Default.finest(s"read($a,$b)")
      abs!(a, b)
    }
    if (logging) Default.finer(s"zip($as,$bs)($abs) terminated")
    as.closeIn()
    bs.closeIn()
    abs.closeOut()
  }

  def source[T](out: OutPort[T], it: Iterable[T]): proc = proc (s"source($out,...)"){
      repeatFor (it) { t => out!t }
      out.closeOut()
  }

  def sink[T](in: InPort[T])(andThen: T=>Unit): proc = proc (s"sink($in)"){
      repeatedly { in?andThen }
      in.closeIn()
  }
}

