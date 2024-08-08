package org.sufrin.microCSO

/**
 * A somewhat simplified sublanguage of ThreadCSO using only
 * virtual threads, and with only minimal debugger support.
 *
 */

import org.sufrin.logging._
import org.sufrin.microCSO.termination._
import org.sufrin.microCSO.Time.{microSec, Nanoseconds}
import org.sufrin.microCSO.altTools._
import org.sufrin.microCSO.proc.{repeatedly, stop}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


trait Closeable {
  /** Close this, and give up any of its resources */
  def close(): Unit
}

/** Outcome of a tentative read/write */
trait  AltOutcome {}
class  AltResult[T] extends AtomicReference[T]

object AltOutcome {
  case object OK extends AltOutcome
  case object CLOSED extends AltOutcome
  case object NO extends AltOutcome
}

trait OutPort[T] extends Closeable { port =>
  /**
   * Idempotent promise that there will be no further writes to this channel.
   * Buffered channels may nevertheless be capable of further reads.
   */
  def closeOut(): Unit
  def close(): Unit = closeOut()

  def writeBefore(timeoutNS: Time.Nanoseconds)(value: T): Boolean
  def !(t: T): Unit
  
  /**
   * Behaves as `!(t())` and yields `OK` if something can be accepted by the channel; else
   * yields `NO` or `CLOSED`.
   */
  def offer(t: () => T): AltOutcome
  
  def out: OutPort[T] = port

  /** This port hasn't yet been closed */
  def canWrite: Boolean

  def isSharedOutPort: Boolean = false

  /**
   * OutPort event notation
   */
  def && (guard: => Boolean): GuardedPort[T] = GuardedPort[T](()=>guard, Out(out))
  def =!=> (value: =>T): altTools.!![T]  = altTools.`!!`(()=>true, out, ()=>value)
}


trait InPort[T] extends Closeable { port: InPort[T] =>
  /** Idempotent promise that there will be no further reads from the associated channel */
  def closeIn(): Unit
  def close(): Unit = closeIn()
  def readBefore(timeOut: Time.Nanoseconds): Option[T]
  /** read from the associated channel */
  def ?(t: Unit): T
  /** read from the associated channel and apply `f` to the result */
  def ?[V](f: T=>V): V = f(this.?(()))
  /**
   *   Extended-rendezvous read: return to the writer only when `f` terminates.
   *   This is distinct from `?[V](f: T=>V): V` only in synchronized channels.
   */
  def ??[V](f: T=>V): V = f(this.?(()))
  /** this port itself */
  def in: InPort[T] = port

  /**
   * Behaves as `result.set(?())` and yields `OK` when a writer has already committed to output
   * else yields `CLOSED` or `NO`
   */
  def poll(result: AltResult[T]): AltOutcome

  /** This port hasn't yet been closed */
  def canRead: Boolean

  def isSharedInPort: Boolean = false

  /**
   *  InPort event notation
   */
  def && (guard: => Boolean): GuardedPort[T] = GuardedPort[T](()=>guard, In(in))
  def =?=> (f: T=>Unit): altTools.??[T]  = altTools.`??`(()=>true, in, f)
}

case class GuardedPort[T](guard: () => Boolean, port: Port[T]) {
  def =?=> (f: T=>Unit): altTools.??[T]  = altTools.`??`(()=>true, port.asIn, f)
  def =!=> (value: =>T): altTools.!![T]  = altTools.`!!`(guard, port.asOut, ()=>value)
}


object Time {
  type Nanoseconds = Long
  type Milliseconds = Long

  /** Number of nanoseconds in a nanosecond */
  val nanoSec: Nanoseconds = 1L

  /** Number of nanoseconds in a microsecond: `n*microSec` is n microseconds
   * expressed as nanoseconds
   */
  val microSec: Nanoseconds = 1000L * nanoSec

  /** Number of nanoseconds in a microsecond: `n*μS` is n microseconds expressed
   * as nanoseconds
   */
  val μS: Nanoseconds = microSec

  /** Number of nanoseconds in a millisecond: `n*milliSec` is n milliseconds
   * expressed as nanoseconds
   */
  val milliSec: Nanoseconds = 1000L * microSec

  /** Number of nanoseconds in a second: `n*Sec` is n seconds expressed as
   * nanoseconds
   */
  val Sec: Nanoseconds = 1000L * milliSec

  /** Number of nanoseconds in a minute: `n*Min` is n minutes expressed as
   * nanoseconds
   */
  val Min: Nanoseconds = 60L * Sec

  /** Number of nanoseconds in an hour: `n*Hour` is n hours expressed as
   * nanoseconds
   */
  val Hour: Nanoseconds = 60L * Min

  /** Number of nanoseconds in a day: `n*Day` is n days expressed as nanoseconds
   */
  val Day: Nanoseconds = 24L * Hour

  /** Convert a fractional time expressed in seconds to nanoseconds */
  def seconds(secs: Double): Nanoseconds = (secs * Sec).toLong

  /** Sleep for the given number of milliseconds. */
  @inline def sleepms(ms: Milliseconds): Unit = Thread.sleep(ms)

  /** Sleep for the given number of nanoseconds */
  @inline def sleep(ns: Nanoseconds): Unit =
    Thread.sleep(ns / milliSec, (ns % milliSec).toInt)

  /** Read the system nanosecond timer */
  @inline def nanoTime: Nanoseconds = System.nanoTime()

  /** Read the system millisecond timer */
  @inline def milliTime: Milliseconds = System.currentTimeMillis()

  /** Wait until `deadline` for `condition` to become true. If it became true
   * before the deadline then the result is the time remaining when it became
   * true. Otherwise the result will be negative, and representing the time
   * after the deadline when deadline expiry was noticed.
   *
   * @param blocker
   *   the object to be reported as the blocker by debuggers
   * @param deadline
   *   the deadline in nanoseconds
   * @param condition
   *   the condition
   * @return
   *   Nanoseconds remaining when the condition became true or when the
   *   deadline expired (possibly negative)
   */
  @inline def parkUntilDeadlineOr(
                                   blocker: AnyRef,
                                   deadline: Nanoseconds,
                                   condition: => Boolean
                                 ): Nanoseconds = {
    var left = deadline - System.nanoTime
    while (left > 0 && !condition) {
      java.util.concurrent.locks.LockSupport.parkNanos(blocker, left)
      left = deadline - System.nanoTime
    }
    // left<=0 || condition
    return left
  }

  /** Equivalent to `parkUntilDeadline(blocker, timeOut+System.nanoTime,
   * condition)`
   */
  @inline def parkUntilElapsedOr(
                                  blocker: AnyRef,
                                  timeOut: Nanoseconds,
                                  condition: => Boolean
                                ): Nanoseconds = {
    val deadline = timeOut + System.nanoTime
    var left = timeOut
    while (left > 0 && !condition) {
      java.util.concurrent.locks.LockSupport.parkNanos(blocker, left)
      left = deadline - System.nanoTime
    }
    // left<=0 || condition
    return left
  }


}


/** Database of running processes, and channels   */
object RuntimeDatabase {
  def reset(): Unit = {
    vThreads.clear()
    vChannels.clear()
  }
  /** Mapping of (running) thread ids to RuntimeDatabase */
  val vThreads =
    new scala.collection.concurrent.TrieMap[Long, Thread]

  val vChannels =
    new scala.collection.concurrent.TrieMap[Int, AnyRef]

  /** Evaluate f at each of the running threads  */
  def forEach(f: Thread=>Unit): Unit =
    vThreads.foreach{ case (_, thread) => f(thread)}

  /** remove from the database (when terminating) */
  def remove(thread: Thread): Unit = {
    vThreads.remove(thread.threadId)
    thread.getState match {
      case Thread.State.TERMINATED => removeLocals(thread)
      case _ =>
    }
  }

  /** add to the database (when starting) */
  def add(thread: Thread): Unit = vThreads += (thread.threadId -> thread)

  /** Evaluate f at each of the running threads  */
  def forEachChannel(f: Chan[Any] => Unit): Unit =
    vChannels.foreach{ case (_, chan) => f(chan.asInstanceOf[Chan[Any]])}

  /** remove from the database (when terminating) */
  def removeChannel(chan: Chan[_]): Unit = vChannels.remove(System.identityHashCode(chan))

  /** add to the database (when starting) */
  def addChannel(chan: Chan[_]): Unit = vChannels += (System.identityHashCode(chan) -> chan)

  //////////////////////// local variables

  type LocalKey = String
  type LocalThunk = ()=> Any

  val vLocals =
    new scala.collection.concurrent.TrieMap[Long, scala.collection.concurrent.TrieMap[LocalKey, LocalThunk]]

  def newLocal[V](key: String, value: =>V): Unit = {
    val id  = Thread.currentThread().threadId
    val map = vLocals.getOrElseUpdate(id, new scala.collection.concurrent.TrieMap[LocalKey, LocalThunk])
    map += (key -> { () => value })
  }

  def forLocals(thread: Thread)(fun: (String, Any)=>Unit): Unit = {
    val id  = thread.threadId
    for { map <- vLocals.get(id) }
        for { (k, thunk) <- map } fun(k, thunk())
  }

  def removeLocals(thread: Thread): Unit = {
    val id  = thread.threadId
    for { map <- vLocals.get(id) } map.clear()
    vLocals.remove(id)
  }
}

object Threads {
  import java.io.PrintStream

  val suppress: String = "java.base"


  def showThreadTrace(thread: Thread, out: PrintStream) = {
    out.println(thread)
    RuntimeDatabase.forLocals(thread) {
      case (key, value) => out.println(f"$key%8s -> $value%s")
    }
    showStackTrace(thread.getStackTrace, out)
  }

  def showStackTrace(trace: Array[StackTraceElement], out: PrintStream=System.out) = {
    for (frame <- trace
         if ! frame.isNativeMethod
        )
    {
      if (frame.getClassName.startsWith("java.")) {
      }
      else
        out.println(unmangle(frame.toString))
    }
    out.println()
  }

  def showThreadTrace(thread: Thread): Unit =
      showThreadTrace(thread: Thread, System.out)

  /** Mapping from mangleable characters to their mangling. */
  private val mangleMap = List(
    ("~", "$tilde"),
    ("=", "$eq"),
    ("<", "$less"),
    (">", "$greater"),
    ("!", "$bang"),
    ("#", "$hash"),
    ("%", "$percent"),
    ("^", "$up"),
    ("&", "$amp"),
    ("|", "$bar"),
    ("*", "$times"),
    ("/", "$div"),
    ("+", "$plus"),
    ("-", "$minus"),
    (":", "$colon"),
    ("\\", "$bslash"),
    ("?", "$qmark"),
    ("@", "$at")
  )

  /** unmangle a compiler-generated mangled name */
  private def unmangle(name: String): String = {
    var r = name
    for ((ch, mangled) <- mangleMap) r = r.replace(mangled, ch)
    r
  }
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
  private val logging: Boolean = false
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
      RuntimeDatabase.add(thread)           // add to the database
      prevName = thread.getName
      thread.setName(name)
      status = Running
      body()
      status = Terminated
    } catch {
      case thrown: Anticipated =>
        status = thrown
      case thrown: Throwable   =>
        status = unAnticipated(thrown)
        Default.error(s"[${Thread.currentThread().getName}] threw $thrown")
    } finally {
      thread.setName(prevName)  // remove from the database
      RuntimeDatabase.remove(Thread.currentThread)
    }
    if (logging)
      Default.finest(s"($this).run() => $status")
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
  def terminationStatus(deadline: Time.Nanoseconds): (Boolean, Status) = {
    val terminated =
        if (latch==null) true else latch.await(deadline, TimeUnit.NANOSECONDS)
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
  private val logging: Boolean = false

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
  private val logging: Boolean  = false
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
    if (logging) Default.finest(s"$name terminating (${firstHandle.status}, ${peerHandles.map(_.status).mkString(", ")}")
    if (logging) Default.finest(s"|| status was $status") //**
    status.propagate()
  }

  def fork(): ForkHandle = {
    val handle = new ForkHandle(name, apply, new Latch(1))
    handle.start()
    handle
  }
}

/**
 * Implementation of the algebra of terminal `Status`es that is used to support
 * coherent termination of `||` constructs. `||(P1, ... Pn)` constructs a process
 * that (when started) terminates when all its components have terminated. Its terminal
 * status is the least upper bound of the terminal statuses of its components (in
 * the ordering `unAnticipated >= Anticipated >= Terminated`). This
 * supports the "outward" propagation of non-`Terminated` statuses.
 * // TODO: It may, at some point, be useful to preserve the detail of
 *          the termination statuses; in which case another constructor
 *          can be added.
 */
object termination {
  private val logging: Boolean = false
  trait Status {
    /** LUB of `this` and `other` */
    def |||(other: Status):Status = termination.|||(this, other)
    def propagate(): Unit
  }

  lazy val Stopped: Anticipated = new Anticipated("stop")

  /**
   *  @return an LUB of `l, and `r`
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
      if (logging) {
        if (Default.level>=FINEST) {
          Default.finest(s"[${Thread.currentThread().getName}]$this.propagate()")
          Threads.showStackTrace(throwable.getStackTrace)
          System.out.flush()
        }
      }
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
  private val logging: Boolean = false

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

  def ||(procs: Seq[process]): process = new par(procs)
  def ||(proc: process, procs: process*): process = new par(proc::procs.toList)


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

  /**
   *  Evaluate `body` then yield its result after closing all
   *  the listed ports
   */
  def withPorts[T](ports: Closeable*)(body: =>T): T = {
    var result = null.asInstanceOf[T]
    try {
      result = body
    } catch {
      case exn: Throwable => throw exn
    }
    finally {
      for { port <-ports } port.close()
    }
    result
  }
}



object altTools  {
  private final val nanoDelta: Nanoseconds = 5*microSec

  trait Port[T] {
    def asIn:  InPort[T]  = null
    def asOut: OutPort[T] = null
  }


  case class Out[T](port: OutPort[T]) extends Port[T] { override def asOut = port }
  case class In[T](port: InPort[T]) extends Port[T]   { override def asIn: InPort[T] = port }
  case class InOut[T](chan: Chan[T]) extends Port[T]  {
    override def asIn: InPort[T]   = chan.in
    override def asOut: OutPort[T] = chan.out
  }

  trait Event
  case class `??`[T](guard: () => Boolean, port: InPort[T],  f: T => Unit) extends Event
  case class `!!`[T](guard: () => Boolean, port: OutPort[T], f: () => T) extends Event
  case class `Or-Else`(eval: ()=>Unit)
  case class `After-NS`(ns: Nanoseconds, eval: ()=>Unit)

  import AltOutcome._

  /**
   *  Repeatedly find and fire one of the `events`, until
   *  none are feasible. The search for firing events is in
   *  sequential order: it is "fair" inasmuch as
   *  at each iteration `events` is rotated by a single place.
   *
   *  The "Finding" is done by polling periodically (the period is half a microsecond). This
   *  is somewhat inelegant, but drastically simplifies the implementation.
   *
   * @see ServeBefore
   */
  def Serve(events: Seq[Event]): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline=0, afterDeadline = {()=>}) match {
        case true  => fairness.rot()
        case false => stop
      }
    }
  }
/**
 *  Repeatedly find and fire one of the `events` until
 *  none are feasible. The search for firing events is in
 *  sequential order: it is "fair" inasmuch as
 *  at each iteration `events` is rotated by a single place.
 *
 * The "finding" is performed by polling every `waitDelta` nanoseconds until
 * {{{
 *   (a) one of the `events` has fired -- then rotate `events` and continue serving
 *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline` and continue serving
 *   (c) none of the `events` are feasible -- then evaluate `orElse`, and stop
 * }}}
 */
 def ServeBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Seq[Event]): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline, afterDeadline = {()=>afterDeadline}) match {
        case true  =>
          fairness.rot()
        case false =>
          orElse
          stop
      }
    }
  }

  /**
   * @see ServeBefore
   */
  def serveBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Event*): Unit = {
    val fairness = new Rotater(events)
    repeatedly {
      eventFired(fairness, deadline, afterDeadline = {()=>afterDeadline}) match {
        case true  =>
          fairness.rot()
        case false =>
          orElse
          stop
      }
    }
  }

  /**
   *  Repeatedly find one of the `events` that fires, until
   *  none are feasible. The search for firing events is in
   *  sequential order: it is "fair" inasmuch as
   *  at each iteration `events` is rotated by a single place.
   *
   *  The "Finding" is done by polling periodically (the period is half a microsecond). This
   *  is somewhat inelegant, but drastically simplifies the implementation.
   *
   * @see serveBefore
   */
  def serve(events: Event*): Unit = Serve(events)

  /**
   * Poll periodically until one of the `events` fires or none is feasible.
   * In the latter case an error is thrown.
   *
   * @see altBefore
   */
  def alt(events: Event*): Unit = Alt(events)

  /**
   * Poll periodically until one of the `events` fires or none is feasible. In
   * the latter case an error is thrown.
   *
   * @see AltBefore
   */
  def Alt(events: Seq[Event]): Unit =
      eventFired(events, deadline=0, afterDeadline = {()=>})  match {
        case false => throw new Error("alternation: no feasible events")
        case true =>
      }

  /**
   * Poll every `waitDelta` nanoseconds until
   * {{{
   *   (a) one of the `events` has fired
   *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline`
   *   (c) none of the `events` are feasible -- then evaluate `orElse`.
   * }}}
   *
   */
  def altBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {} , waitDelta: Nanoseconds = nanoDelta)(events: Event*): Unit =
      eventFired(events, deadline, afterDeadline = { ()=>afterDeadline }, waitDelta) match {
        case false => orElse
        case true  =>
      }

  /**
   * Poll every `waitDelta` nanoseconds until
   * {{{
   *   (a) one of the `events` has fired
   *   (b) the deadline (if positive) has expired -- then evaluate `afterDeadline`
   *   (c) none of the `events` are feasible -- then evaluate `orElse`
   * }}}
   *
   */
  def AltBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = nanoDelta )(events: Seq[Event]): Unit =
      eventFired(events, deadline, afterDeadline = { ()=>afterDeadline }, waitDelta) match {
        case false => orElse
        case true  =>
      }

  /**
   *  (busy) wait until one of `events`  fires or none are feasible.
   *  Yield true in the former case and false in the latter case.
   *
   *  If there's a positive `deadline` then
   *  evaluate `afterDeadline()` and return true if none has fired before the
   *  deadline has elapsed; else just continue (busy) waiting.
   *
   *  `waitDelta` is the delay between successive attempts to find and fire an event.
   *
   */
  def eventFired(events: Seq[Event], deadline: Nanoseconds=0, afterDeadline: ()=>Unit, waitDelta: Nanoseconds = nanoDelta): Boolean = {
    var outcome = false
    var waiting = true
    var remainingTime = deadline
    val hasDeadline   = remainingTime>0

    while (waiting) {
      val (feasibles, result) = fireFirst(events)
      // result is OK or all events refused or infeasible
      result match {
        case OK =>
          // an event fired
          outcome = true
          waiting = false
        case CLOSED | NO =>
          // no event seen on this pass fired
          if (feasibles == 0) {
            // now nothing CAN happen
            // print("* "); System.out.flush()
            outcome = false
            waiting = false
          } else if (hasDeadline && remainingTime<=0) {
            afterDeadline()
            outcome = true
            waiting = false
          } else {
            // wait a bit and try again
            Time.sleep(waitDelta)
            remainingTime -= waitDelta
          }
      }
    }
    outcome
  }

  /**
   * Find and fire the first ready event.
   * Return the number of feasible events.
   */
  @inline private final def fireFirst(events: Seq[Event]): (Int, AltOutcome) = {
    var outcome:  AltOutcome = NO
    var feasible: Int = 0
    val feasibles = events.filter{
      case `!!`(guard, port, _) => guard() && port.canWrite
      case `??`(guard, port, _) => guard() && port.canRead
    }
    //if (feasibles.isEmpty) Default.finer(s"fireFirst ${feasibles.length} feasible")
    val iter: Iterator[Event] = feasibles.iterator
    // find and fire the first ready event
    while (outcome!=OK && iter.hasNext) {
      iter.next() match {
        case `!!`(guard, port, f) =>
            outcome = port.offer(f)
            outcome match {
              case OK =>
                feasible += 1
              case CLOSED =>
                outcome = CLOSED
              case NO     =>
                outcome = NO            }
        case `??`(guard, port, f) =>
            val result = new AltResult[Any]
            port.poll(result) match {
              case OK     =>
                f(result.get)
                outcome = OK
                feasible += 1
              case CLOSED =>
                outcome = CLOSED
              case NO     =>
                outcome = NO
            }
      }
    }
    // outcome==OK || all events refused or infeasible // TODO: need to distinguish?
    // Default.finer(s"fireFirst $feasible/${feasibles.length} $outcome")
    (feasibles.length, outcome)
  }

  class Rotater[T](val original: Seq[T]) extends Seq[T] {
    val length: Int = original.length
    private val LENGTH = length
    private var offset: Int = 0
    def rot(): Unit = { offset = (offset+1) % LENGTH}

    def iterator: Iterator[T] = new Iterator[T] {
      var current: Int = offset
      var count : Int  = LENGTH
      def hasNext: Boolean = count>0

      def next(): T = {
        val r = original(current)
        current = (current + 1) % LENGTH
        count -=1
        r
      }
    }

    def apply(i: Int): T = original((offset+i)%LENGTH)
  }
}

object portTools extends Loggable{
  import proc._


  def zip[A,B](as: InPort[A], bs: InPort[B])(out: OutPort[(A,B)]): proc = proc(s"zip($as,$bs)($out)") {
    var a = null.asInstanceOf[A]
    var b = null.asInstanceOf[B]
    val read = proc(s"$as?()") {
      a = as ? ()
    } || (proc(s"$bs?()") {
      b = bs ? ()
    })
    withPorts(as, bs, out) {
      repeatedly {
        read()
        out ! (a, b)
      }
    }
  }

  def zip[A,B,C](as: InPort[A], bs: InPort[B], cs: InPort[C])(out: OutPort[(A,B,C)]): proc = proc(s"zip($as,$bs,$cs)($out)") {
    var a = null.asInstanceOf[A]
    var b = null.asInstanceOf[B]
    var c = null.asInstanceOf[C]
    val read =
        ||(proc(s"$as?()") { a = as ? () },
           proc(s"$bs?()") { b = bs ? () },
           proc(s"$cs?()") { c = cs ? () })
    withPorts(as, bs, cs, out) {
      repeatedly {
        read()
        out ! (a, b, c)
      }
    }
  }

  def copy[T](in: InPort[T], out: OutPort[T]): process = proc(s"copy($in, $out)") {
    withPorts(in, out) {
      repeatedly {
        out ! (in ? ())
      }
    }
  }

  def merge[T](ins: Seq[InPort[T]])(out: OutPort[T]): process  = proc (s"merge($ins)($out)") {
      Serve(
        for { in<-ins } yield in =?=> { t => out!t }
      )
    if (logging) finer(s"merge($ins)($out) SERVE terminated")
    out.closeOut()
    for { in<-ins } in.closeIn()
    if (logging) finer(s"merge($ins)($out) terminated after closing")
  }

  def source[T](out: OutPort[T], it: Iterable[T]): proc = proc (s"source($out,...)"){
      var count = 0
      //RuntimeDatabase.newLocal("source count", count)
      //RuntimeDatabase.newLocal("source out", out)
      repeatFor (it) { t => out!t; count += 1 }
      out.closeOut()
      if (logging) finer(s"source($out) closed")
  }

  def sink[T](in: InPort[T])(andThen: T=>Unit): proc = proc (s"sink($in)"){
      var count = 0
      //RuntimeDatabase.newLocal("sink count", count)
      //RuntimeDatabase.newLocal("sink in", in)
      repeatedly { in?{ t => andThen(t); count+=1 } }
      in.closeIn()
      if (logging) finer(s"sink($in) closed")
  }
}

