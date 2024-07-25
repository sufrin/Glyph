package org.sufrin.microCSO

/**
 * A drastically simplified sublanguage of ThreadCSO using only
 * virtual threads.
 */

import org.sufrin.logging._
import org.sufrin.microCSO.termination._
import org.sufrin.microCSO.PortState.{CLOSEDSTATE, IDLESTATE, PortState, READYSTATE}
import org.sufrin.microCSO.Time.Nanoseconds

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.locks.LockSupport


trait Closeable {
  /** Close this, and give up any of its resources */
  def close(): Unit
}

trait OutPort[T] extends Closeable { port =>
  /**
   * Idempotent declaration that there will be no further writes to this channel.
   * Buffered channels may nevertheless be capable of further reads.
   */
  def closeOut(): Unit
  def close(): Unit = closeOut()

  def writeBefore(timeoutNS: Time.Nanoseconds)(value: T): Boolean
  def !(t: T): Unit
  def out: OutPort[T] = port
  /** This port hasn't yet been closed */
  def canWrite: Boolean
  /**
   *  The state of the channel underlying this port changed, leaving
   *  the port itself in `newState` -- used only in alternation
   *  constructions, and must be overridden in alternation-potent
   *  channels.
   */
  def outPortEvent(newState: PortState.PortState): Unit = {}
  def outPortState: PortState.PortState
}

trait InPort[T] extends Closeable { port =>
  /** Idempotent declaration that there will be no further reads from this channel */
  def closeIn(): Unit
  def close(): Unit = closeIn()
  def readBefore(timeOut: Time.Nanoseconds): Option[T]
  def ?(t: Unit): T
  def ?[V](f: T=>V): V = f(this.?(()))
  /**
   *   Extended-rendezvous read: return to the writer only when `f` terminates.
   *   This is distinct from `?[V](f: T=>V): V` only in synchronized channels.
   */
  def ??[V](f: T=>V): V = f(this.?(()))
  def in: InPort[T] = port
  /** This port hasn't yet been closed */
  def canRead: Boolean
  /**
   *  The state of the channel underlying this port changed, leaving
   *  the port itself in `newState` -- used only in alternation
   *  constructions, and must be overridden in alternation-potent
   *  channels.
   */
  def inPortEvent(newState: PortState.PortState): Unit = {}
  def inPortState: PortState.PortState
}

trait Chan[T] extends OutPort[T] with InPort[T] {
  /** Capture (an approximation to) the current state for debugger components */
  def currentState: String

  /** Close both ports of this channel  */
  override def close(): Unit = {
    closeIn()
    closeOut()
  }
}

object PortState {
  trait PortState
  case object READYSTATE   extends PortState
  case object CLOSEDSTATE  extends PortState
  case object IDLESTATE extends PortState
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

/**
 * A non-shared synchronized (unbuffered) channel with the given `name`. When `c` is
 * such a channel, the termination of `c!v` (in a writing process) is synchronized with that
 * of `c?()` in a (distinct) reading process. The event is called a rendezvous: thje first
 * process to arrive at the rendezvous awaits the second. If more than one reader arrives
 * before a writer (or vice-versa) the channel detects that it is being shared; but this
 * heuristic for detecting sharing is not complete, because a consumer that repeatedly
 * consumes at a faster rate than two producers will not be detectable this way.
 *
 * @param name
 * @tparam T
 */
class SyncChan[T](name: String) extends Chan[T] {

  import PortState._
  private[this] val reader, writer = new AtomicReference[Thread]
  private[this] val closed, full   = new AtomicBoolean(false)
  private[this] var buffer: T = _

  locally { RuntimeDatabase.addChannel(this) }

  /** READY if not closed and there's a writer waiting to sync (else CLOSED or
   * IDLE) (used only by alternation implementations)
   */
  def inPortState: PortState =
    if (closed.get) CLOSEDSTATE else if (full.get) READYSTATE else IDLESTATE

  /** READY if not closed and there's a reader waiting and last writer has
   * already synced (else CLOSED or IDLE) (used only by alternation
   * implementations)
   */
  def outPortState: PortState =
    if (closed.get) CLOSEDSTATE
    else if (!full.get && reader.get != null) READYSTATE
    else IDLESTATE


  /**
   *  Capture (an approximation to) the current state for debugger components
   */
  def currentState: String = {
    val wr = reader.get
    val ww = writer.get
    val result =
      if (ww == null && wr == null) "(IDLING)"
      else {
        if (ww != null)
          if (full.get)
            s"!($buffer) from ${ww.getName}"
          else
            s"! from ${ww.getName}"
        else
          s"? from ${wr.getName}"
      }
    result
  }

  override def toString: String =
    s"""SyncChan.${this.name} [in: $inPortState, out: $outPortState] $currentState"""

  def !(value: T) = {
    checkOpen
    val current = Thread.currentThread
    val lastWriter = writer.getAndSet(current)
    assert(
      lastWriter == null,
      s"[${current.getName}]$name!($value) overtaking [${lastWriter.getName}]$name!($buffer)"
    )
    buffer = value
    full.set(true)
    inPortEvent(READYSTATE)         // Announce state change to any alternation
    LockSupport.unpark(reader.get)  // DELIVER BUFFER TO READER
    while (!closed.get && full.get) // AWAIT SYNC FROM READER
    {
      LockSupport.park(this)
    }
    if (full.get) checkOpen         // *** (see close) []
    writer.set(null)                // READY
  }

  def ?(t: Unit): T = {
    checkOpen
    val current     = Thread.currentThread
    val lastReader  = reader.getAndSet(current)
    assert(
      lastReader == null,
      s"[${current.getName}]$name?() overtaking [${lastReader.getName}]$name?()"
    )
    outPortEvent(READYSTATE)          // Announce state change to any alternation
    while (!closed.get && !full.get)  // AWAIT BUFFER
    {
      LockSupport.park(this)
    }
    checkOpen                       // ** (see close)
    val result = buffer             // ## (cf. ?? at ##)
    buffer = null.asInstanceOf[T]   // For the garbage-collector
    reader.set(null)
    full.set(false)                 // EMPTY BUFFER; READY
    LockSupport.unpark(writer.get)  // SYNC WRITER
    result
  }

  /**
   * Fully close this channel for both input and output.
   */
  override def close(): Unit = {
    if (!closed.getAndSet(true)) { // closing is idempotent
      outPortEvent(CLOSEDSTATE)    // Announce state change
      inPortEvent(CLOSEDSTATE)     // Announce state change
      LockSupport.unpark(
        reader.getAndSet(null)
      ) // Force a waiting reader to continue **
      LockSupport.unpark(
        writer.getAndSet(null)
      ) // Force a waiting writer to continue ***
      RuntimeDatabase.removeChannel(this)// Debugger no longer interested
    }
  }

  def canRead: Boolean  = !closed.get
  def canWrite: Boolean = !closed.get

  /** Extended rendezvous read & compute, then sync */
  override def ??[U](f: T => U): U = {
    checkOpen
    val current = Thread.currentThread
    val lastReader = reader.getAndSet(current)
    assert(
      lastReader == null,
      s"[${current.getName}]$name??() overtaking [${lastReader.getName}]$name??()"
    )
    outPortEvent(READYSTATE)
    while (!closed.get && !full.get) {
      LockSupport.park(this)
    }
    checkOpen                     // ** (see close)
    val result = f(buffer)        // ## compute before the write sync: (cf. ? at ##)
    buffer = null.asInstanceOf[T] // For the garbage collector
    reader.set(null)
    full.set(false)               // EMPTY BUFFER; READY
    LockSupport.unpark(writer.get)// SYNC WRITER
    result
  }

  def canInput: Boolean = !closed.get

  def closeIn(): Unit = close()

  def canOutput: Boolean = !closed.get

  def closeOut(): Unit = close()

  @inline private[this] def checkOpen =
    if (closed.get) {
      writer.set(null)
      reader.set(null)
      throw new Closed(name)
    }

  def readBefore(timeoutNS: Time.Nanoseconds): Option[T] = {
    val current = Thread.currentThread
    assert(
      reader.get == null,
      s"[${current.getName}]$name?() overtaking [${reader.get.getName}]$name?()"
    )
    checkOpen
    reader.set(current)
    outPortEvent(READYSTATE)
    val success =
      0 < Time.parkUntilElapsedOr(this, timeoutNS, closed.get || full.get)
    checkOpen
    val result = buffer
    buffer = null.asInstanceOf[T]
    reader.set(null)
    full.set(false)
    LockSupport.unpark(writer.get)
    if (success) Some(result) else None
  }

  def writeBefore(timeoutNS: Time.Nanoseconds)(value: T): Boolean = {
    assert(
      writer.get == null,
      s"$name!($value) from ${Thread.currentThread.getName} overtaking $name!($buffer) [${writer.get.getName}]"
    )
    checkOpen
    buffer = value
    val current = Thread.currentThread
    writer.set(current)
    full.set(true)
    inPortEvent(READYSTATE)
    LockSupport.unpark(reader.getAndSet(null))
    var success =
      0 < Time.parkUntilElapsedOr(this, timeoutNS, closed.get || !full.get)
    if (!success) full.set(false)
    writer.set(null)
    checkOpen
    success
  }

}

/**
 *  A shareable synchronized channel (for completeness). Sharing affects only the
 *  port-closing methods: readers/writers may not coincide at a rendezvous.
 */
class SharedSyncChan[T](name: String, readers: Int=1, writers: Int = 1) extends SyncChan[T](name) {
  import java.util.concurrent.locks.{ReentrantLock => Lock}
  val rLock, wLock = new Lock()

  @inline final def withLock[T](lock: Lock) ( body: => T ): T = {
    lock.lock()
    try { body } catch { case exn: Throwable => throw exn } finally { lock.unlock() }
  }

  val readersLeft = new AtomicLong(readers)
  val writersLeft = new AtomicLong(writers)

  override def toString: String = s"SharedSyncChan.$name  [in: $inPortState, out: $outPortState][readers=${readersLeft.get}, writers=${writersLeft.get}] $currentState"

  override def closeIn(): Unit = withLock(rLock) {
    if (readersLeft.decrementAndGet()<=0) super.close()
  }

  override def closeOut(): Unit = withLock (wLock) {
    if (writersLeft.decrementAndGet()<=0) super.close()
  }

  override def ?(u: Unit): T = withLock(rLock) { super.?(()) }
  override def !(t: T): Unit = withLock(wLock) { super.!(t) }

  override def readBefore(timeoutNS: Nanoseconds): Option[T] = withLock(rLock) { super.readBefore(timeoutNS) }
  override def writeBefore(timeoutNS: Nanoseconds)(value: T) = withLock(wLock) { super.writeBefore(timeoutNS)(value) }

}

/**
 * A (potentially-shared) buffered channel with the given name and `capacity`, expected to
 * be closed for input (exactly) `readers` times; and for output
 * exactly `writers` times. In these situations we say the channel is
 * "fully closed" (for input / for output).
 *
 * Reads will continue to succeed while the buffer is nonempty -- even
 * when the channel is fully closed for output. Writes will succeed if
 * the channel is neither fully closed for input or for output.
 *
 * @param name
 * @param capacity
 * @param readers
 * @param writers
 * @tparam T
 */
class SharedBufferedChan[T](name: String, capacity: Int, readers: Int=1, writers: Int = 1) extends Chan[T] {
  import termination._
  override val toString: String = s"SharedBuffered.$name($capacity)"
  locally { RuntimeDatabase.addChannel(this) }

  def currentState: String = s"$name capacity=${buffer.remainingCapacity()} readers=${waitingReader.get} writers=${waitingWriter.get}"

  val inOpen, outOpen   = new AtomicBoolean(true)
  def canRead: Boolean  = inOpen.get
  def canWrite: Boolean = outOpen.get

  /** When non-null this is a peer waiting to write/read */
  val waitingWriter, waitingReader = new AtomicReference[Thread]
  val readersLeft = new AtomicLong(readers)
  val writersLeft = new AtomicLong(writers)

  def inPortState: PortState =
    if (!inOpen.get) CLOSEDSTATE else if (buffer.isEmpty) IDLESTATE else READYSTATE

  def outPortState: PortState =
    if (!outOpen.get) CLOSEDSTATE else if (buffer.remainingCapacity()==0) IDLESTATE else READYSTATE

  def closeIn(): Unit = {
    if (readersLeft.decrementAndGet()==0) CloseIn()
  }

  def closeOut(): Unit = {
    if (writersLeft.decrementAndGet()==0) CloseOut()
  }

  /** Close the input port unconditionally, and interrupt any waiting writer: idempotent */
  def CloseIn(): Unit  = {
    Default.finest(s"$this CloseIn()")
    if (inOpen.getAndSet(false)) {
       // it was open
       RuntimeDatabase.removeChannel(this)
       val peer = waitingWriter.getAndSet(null)
       if (peer ne null) peer.interrupt()
    }
    if (completelyClosed) RuntimeDatabase.removeChannel(this)
  }

  /** Close the output port unconditionally, and tell readers: idempotent*/
  def CloseOut(): Unit = {
    Default.finest(s"$this CloseOut()")
    if (outOpen.getAndSet(false)) {
      val peer = waitingReader.getAndSet(null)
      if (peer ne null) peer.interrupt()
    }
    if (completelyClosed) RuntimeDatabase.removeChannel(this)
  }

  @inline private final def completelyClosed: Boolean = !canRead && !canWrite

  val buffer = new LinkedBlockingQueue[T](capacity)

  def readBefore(timeoutNS: Time.Nanoseconds): Option[T] =
      if (inOpen.get) {
        val msg = buffer.poll(timeoutNS, TimeUnit.NANOSECONDS)
        if (msg==null)
           None
        else
           Some(msg)
      }
      else
        throw new Closed(name)

  def writeBefore(timeoutNS: Time.Nanoseconds)(value: T): Boolean =
    if (outOpen.get) {
      buffer.offer(value, timeoutNS, TimeUnit.NANOSECONDS)
    }
    else
      throw new Closed(name)

  def !(t: T): Unit = {
      if (!(inOpen.get && outOpen.get)) throw new Closed(name) else
      if (buffer.offer(t)) {
        waitingWriter.set(null)
      } else {
        waitingWriter.set(Thread.currentThread())
        try { buffer.put(t) } catch { case _: InterruptedException => closeOut() }// perhaps interrupted
        waitingWriter.set(null)
      }
    }

  def ?(t: Unit): T =
      if (inOpen.get) {
        if (!outOpen.get && buffer.isEmpty) {
          // output is closed and the buffer is empty
          throw new Closed(name)
        } else {
          // output is not closed or the buffer has something waiting
          val t: T = buffer.poll()
          try {
            if (t == null) {
              waitingReader.set(Thread.currentThread())
              val w: T = buffer.take() // this may be interrupted
              waitingReader.set(null)
              w
            } else {
              waitingReader.set(null)
              t
            }
          }
          catch {
            case _: InterruptedException =>
              waitingReader.set(null)
              closeIn()
              throw new Closed(name)
          }
        }
      } else
        throw new Closed(name)
}


object Chan extends serialNamer {
  val namePrefix: String = "Chan"
  import org.sufrin.logging._
  def apply[T](capacity: Int): Chan[T] = capacity match {
      case 0 =>
        new SyncChan[T](nextName())
      case _ =>
        new SharedBufferedChan[T](nextName(), capacity, readers=1, writers=1)
  }

  def apply[T](name: String, capacity: Int): Chan[T] = capacity match {
    case 0 =>
      new SyncChan[T](name)
    case _ =>
      new SharedBufferedChan[T](name, capacity, readers=1, writers=1)
  }

  trait SharedChanGenerator {
    def readers: Int
    def writers: Int

    def apply[T](capacity: Int): Chan[T] = capacity match {
      case 0 =>
        new SharedSyncChan[T](nextName(), readers, writers)
      case _ =>
        new SharedBufferedChan[T](nextName(), capacity, readers = readers, writers = writers)
    }

    def apply[T](name: String, capacity: Int): Chan[T] = capacity match {
      case 0 =>
        new SharedSyncChan[T](name)
      case _ =>
        new SharedBufferedChan[T](name, capacity, readers = readers, writers = writers)
    }
  }

  /**
   * `Chan.Shared(readers, writers)(name, capacity)` yields a shared
   * channel of the given capacity, intended to be read by the
   * given number of readers and written by the given number
   * of writers. Closing of the shared channel in a given direction
   * takes place when the given number of `closeIn()`  (resp `closeOut()`)
   * have been invoked.
   *
   * When its capacity is zero, the channel is a synced channel, and may not
   * engage with more than a single reader or writer in the same rendezvous.
   */
  object Shared extends SharedChanGenerator {
    def readers: Int = 0
    def writers: Int = 0

    def apply(readers: Int, writers: Int): SharedChanGenerator = {
      val withReaders=readers
      val withWriters=writers
      new SharedChanGenerator {
        def readers: Int = withReaders
        def writers: Int = withWriters
      }
    }
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
  def remove(thread: Thread): Unit = vThreads.remove(thread.threadId)

  /** add to the database (when starting) */
  def add(thread: Thread): Unit = vThreads += (thread.threadId -> thread)

  /** Evaluate f at each of the running threads  */
  def forEachChannel(f: Chan[Any] => Unit): Unit =
    vChannels.foreach{ case (_, chan) => f(chan.asInstanceOf[Chan[Any]])}

  /** remove from the database (when terminating) */
  def removeChannel(chan: Chan[_]): Unit = vChannels.remove(System.identityHashCode(chan))

  /** add to the database (when starting) */
  def addChannel(chan: Chan[_]): Unit = vChannels += (System.identityHashCode(chan) -> chan)

}

object Threads {
  import java.io.PrintStream

  val suppress: String = "java.base"


  def showThreadTrace(thread: Thread, out: PrintStream) = {
    println(thread)
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
    out.println("")
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
  private val logging: Boolean = true
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

object portTools {
  private val logging: Boolean = false
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
        if (logging) Default.finest(s"read($a,$b)")
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
        if (logging) Default.finest(s"read($a,$b,$c)")
        out ! (a, b, c)
      }
      if (logging) Default.finer(s"zip($as,$bs,$cs)($out) terminated")
    }
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

