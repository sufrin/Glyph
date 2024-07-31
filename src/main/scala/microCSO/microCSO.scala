package org.sufrin.microCSO

/**
 * A somewhat simplified sublanguage of ThreadCSO using only
 * virtual threads, and with only minimal debugger support.
 *
 */

import org.sufrin.logging._
import org.sufrin.microCSO.termination._
import org.sufrin.microCSO.Time.Nanoseconds
import org.sufrin.microCSO.altTools._
import org.sufrin.microCSO.proc.{repeatedly, stop}

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.locks.LockSupport


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
  Default.fine(s"GuardedPort(..., $port)")
  def =?=> (f: T=>Unit): altTools.??[T]  = altTools.`??`(()=>true, port.asIn, f)
  def =!=> (value: =>T): altTools.!![T]  = altTools.`!!`(guard, port.asOut, ()=>value)
}


trait Chan[T] extends OutPort[T] with InPort[T] {
  /** Capture (an approximation to) the current state for debugger components */
  def currentState: String

  /** Close both ports of this channel */
  override def close(): Unit = {
    closeIn()
    closeOut()
  }

  /** The guard notation (from a channel) supports either outport or inport events  */
  override def &&(guard: => Boolean): GuardedPort[T] =
           GuardedPort[T](() => guard, InOut(this))
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

  private[this] val reader, writer = new AtomicReference[Thread]
  private[this] val closed, full   = new AtomicBoolean(false)
  private[this] var buffer: T = _

  locally { RuntimeDatabase.addChannel(this) }





  /**
   *  Capture (an approximation to) the current state for debugger components
   */
  def currentState: String = {
    val wr = reader.get
    val ww = writer.get
    val result =
      if (ww == null && wr == null) "-"
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
    s"""SyncChan.${this.name} $currentState"""

  /**
   *  Behaves as !(t()) when a reader is already committed
   */
  def offer(t: ()=>T): AltOutcome = {
    if (closed.get) AltOutcome.CLOSED else
    if (full.get)   AltOutcome.NO else {
      val peer = reader.get()
      if (peer==null) AltOutcome.NO else {
        val value = t()
        this.!(value)
        AltOutcome.OK
      }
    }
  }

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
    LockSupport.unpark(reader.get)  // DELIVER BUFFER TO READER
    while (!closed.get && full.get) // AWAIT SYNC FROM READER
    {
      LockSupport.park(this)
    }
    if (full.get) checkOpen         // *** (see close) []
    writer.set(null)                // READY
  }

  /** Behaves as `result.set(?())` when  a writer is already committed */
  def poll(result: AltResult[T]): AltOutcome = {
    if (closed.get)
      AltOutcome.CLOSED
    else
    if (full.get) {
      result.set(this.?(()))
      AltOutcome.OK
    }
    else
      AltOutcome.NO
  }

  def ?(t: Unit): T = {
    checkOpen
    val current     = Thread.currentThread
    val lastReader  = reader.getAndSet(current)
    assert(
      lastReader == null,
      s"[${current.getName}]$name?() overtaking [${lastReader.getName}]$name?()"
    )
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
      throw new termination.Closed(name)
    }

  def readBefore(timeoutNS: Time.Nanoseconds): Option[T] = {
    val current = Thread.currentThread
    assert(
      reader.get == null,
      s"[${current.getName}]$name?() overtaking [${reader.get.getName}]$name?()"
    )
    checkOpen
    reader.set(current)
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
 *  A shareable synchronized channel (for completeness) made from an
 *  underlying synchronized channel. Processes
 *  sharing the channel wait (in `Lock` queues) for reading (or writing)
 *  methods to be available. This contrasts with the underlying
 *  channel in which (inadvertently-sharing) readers (writers) MAY (but shouldn't)
 *  overetake each other.
 *
 *  The channel closes at the last invocation of `closeIn`,
 *  or the last invocation of `closeOut`.
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

  override def toString: String = s"SharedSyncChan.$name [readers=${readersLeft.get}/$readers, writers=${writersLeft.get}/$writers] $currentState"

  override def closeIn(): Unit =  {
    if (readersLeft.decrementAndGet()==0) super.close()
  }

  override def closeOut(): Unit = {
    if (writersLeft.decrementAndGet()==0) super.close()
  }

  override def ?(u: Unit): T = withLock(rLock) { super.?(()) }
  override def !(t: T): Unit = withLock(wLock) { super.!(t) }

  override def readBefore(timeoutNS: Nanoseconds): Option[T] = withLock(rLock) { super.readBefore(timeoutNS) }
  override def writeBefore(timeoutNS: Nanoseconds)(value: T) = withLock(wLock) { super.writeBefore(timeoutNS)(value) }
  override def poll(result: AltResult[T]): AltOutcome = withLock(rLock) { super.poll(result) }
  override def offer(t: ()=>T): AltOutcome = withLock(wLock) { super.offer(t) }

}

/**
 * A shared buffered channel with the given name and `capacity`, expected to
 * be closed for input (exactly) `readers` times; and for output
 * exactly `writers` times. In these situations we say the channel is
 * "fully closed" (for input / for output).
 *
 * Either `readers` or `writers` can be zero: in which case the reading (writing)
 * side of the channel remains open until the other side is fully closed. If they
 * are both zero then the channel is never closed by `closeIn()` or `closeOut()`,
 * but can still be closed by `close()`.
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
class SharedBufferedChan[T](name: String, capacity: Int, readers: Int=1, writers: Int=1) extends BufferedChan[T](name, capacity) {
  import java.util.concurrent.locks.{ReentrantLock => Lock}
  val rLock, wLock = new Lock()

  @inline private final def withLock[T](lock: Lock) ( body: => T ): T = {
    lock.lockInterruptibly()
    try { body } finally { lock.unlock() }
  }

  val readersLeft = new AtomicLong(readers)
  val writersLeft = new AtomicLong(writers)

  override def toString: String =
    s"SharedBufferedChan.$name [readers=${readersLeft.get}/$readers, writers=${writersLeft.get}/$writers] {$currentState}"

  override def closeIn(): Unit = {
    Default.finer(s"$this.closeIn()")
    if (readersLeft.decrementAndGet()==0) super.close()
  }

  override def closeOut(): Unit = {
    Default.finer(s"$this.closeOut()")
    if (writersLeft.decrementAndGet()==0) super.close()
  }

  override def ?(u: Unit): T = withLock(rLock) { super.?(()) }
  override def !(t: T): Unit = withLock(wLock) { super.!(t) }

  override def readBefore(timeoutNS: Nanoseconds): Option[T] = withLock(rLock) { super.readBefore(timeoutNS) }
  override def writeBefore(timeoutNS: Nanoseconds)(value: T) = withLock(wLock) { super.writeBefore(timeoutNS)(value) }
  override def poll(result: AltResult[T]): AltOutcome = withLock(rLock) { super.poll(result) }
  override def offer(t: ()=>T): AltOutcome = withLock(wLock) { super.offer(t) }

}

/**
 * An unshared buffered channel with the given `capacity (>0)`.
 *
 * @param name
 * @param capacity
 * @tparam T
 */
class BufferedChan[T](name: String, capacity: Int) extends Chan[T] {
  import termination._

  val buffer = new LinkedBlockingQueue[T](capacity)

  def currentState: String =
    s"$name capacity=${buffer.remainingCapacity()}/${capacity} reader=${waitingReader.get} writer=${waitingWriter.get}"

  val inOpen, outOpen   = new AtomicBoolean(true)
  def canRead: Boolean  = inOpen.get
  def canWrite: Boolean = outOpen.get

  /** When non-null this is a peer waiting to write/read */
  val waitingWriter, waitingReader = new AtomicReference[Thread]

  override def toString: String =
    s"SharedBuffered.$name(${buffer.remainingCapacity()}/$capacity)"

  locally { RuntimeDatabase.addChannel(this) }


  /** Close the channel unconditionally in both directions  */
  override def close(): Unit = {
    closeIn()
    closeOut()
  }

  /** Close the input port unconditionally, and interrupt any waiting writer: idempotent */
  def closeIn(): Unit  =  {
    Default.finest(s"$this CloseIn()")
    if (inOpen.getAndSet(false)) {
       val peer = waitingWriter.getAndSet(null)
       if (peer ne null) peer.interrupt()
    }
    if (completelyClosed) RuntimeDatabase.removeChannel(this)
  }

  /** Close the output port unconditionally, and tell readers: idempotent*/
  def closeOut(): Unit = {
    Default.finest(s"$this CloseOut()")
    if (outOpen.getAndSet(false)) {
      val peer = waitingReader.getAndSet(null)
      if (peer ne null) peer.interrupt()
    }
    if (completelyClosed) RuntimeDatabase.removeChannel(this)
  }

  @inline private final def completelyClosed: Boolean = !canRead && !canWrite

  def readBefore(timeoutNS: Time.Nanoseconds): Option[T] = {
      if (inOpen.get) {
        val msg = buffer.poll(timeoutNS, TimeUnit.NANOSECONDS)
        if (msg==null)
           None
        else
           Some(msg)
      }
      else
        throw new Closed(name)
  }

  def writeBefore(timeoutNS: Time.Nanoseconds)(value: T): Boolean = {
    if (outOpen.get) {
      buffer.offer(value, timeoutNS, TimeUnit.NANOSECONDS)
    }
    else
      throw new Closed(name)
  }

  def offer(t: ()=>T): AltOutcome =  {
    if (!(inOpen.get && outOpen.get))
      AltOutcome.CLOSED
    else
      // ********************* POTENTIAL RACE **********************
      // Safe only when there's a single writer
      // Use the Shared version if you need something more
    if (buffer.remainingCapacity()>0 && buffer.offer(t())) {
      waitingWriter.set(null)
      AltOutcome.OK
    } else
      AltOutcome.NO
  }

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

  def poll(result: AltResult[T]): AltOutcome = {
    if (inOpen.get) {
      if (!outOpen.get && buffer.isEmpty) {
        // output is closed and the buffer is empty
        AltOutcome.CLOSED
      } else {
        val t = buffer.poll()
        if (t==null) AltOutcome.NO else {
          result.set(t)
          AltOutcome.OK
        }
      }
    } else AltOutcome.CLOSED
  }

  def ?(t: Unit): T = {
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
}


object Chan extends serialNamer {
  val namePrefix: String = "Chan"
  import org.sufrin.logging._
  def apply[T](capacity: Int): Chan[T] = capacity match {
      case 0 =>
        new SyncChan[T](nextName())
      case _ =>
        new BufferedChan[T](nextName(), capacity)
  }

  def apply[T](name: String, capacity: Int): Chan[T] = capacity match {
    case 0 =>
      new SyncChan[T](name)
    case _ =>
      new BufferedChan[T](name, capacity)
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
  def remove(thread: Thread): Unit = {
    vThreads.remove(thread.threadId)
    thread.getState match {
      case Thread.State.TERMINATED => vLocals.remove(thread.threadId)
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

  /*
  def setLocal[V](key: LocalKey, value: V): Unit = {
    val id  = Thread.currentThread().threadId
    val map = vLocals.get(id).get
    map += (key->value)
  }

  def getLocal[V](key: LocalKey): V = {
    val id  = Thread.currentThread().threadId
    val map = vLocals.get(id).get
    map.getOrElse(key, null).asInstanceOf[V]
  }
  */

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

object altTools {
  val logging = false

  import AltOutcome._
  trait Event {}
  case class `??`[T](guard: () => Boolean, port: InPort[T],  f: T => Unit) extends Event
  case class `!!`[T](guard: () => Boolean, port: OutPort[T], f: () => T) extends Event
  case class `Or-Else`(eval: ()=>Unit)
  case class `After-NS`(ns: Nanoseconds, eval: ()=>Unit)


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
 def ServeBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = 500000 )(events: Seq[Event]): Unit = {
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
  def serveBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = 500000 )(events: Event*): Unit = {
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
  def altBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {} , waitDelta: Nanoseconds = 500000)(events: Event*): Unit =
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
  def AltBefore(deadline: Nanoseconds=0, afterDeadline: => Unit = {}, orElse: => Unit = {}, waitDelta: Nanoseconds = 500000 )(events: Seq[Event]): Unit =
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
  def eventFired(events: Seq[Event], deadline: Nanoseconds=0, afterDeadline: ()=>Unit, waitDelta: Nanoseconds = 500000): Boolean = {
    var outcome = false
    var waiting = true
    var remainingTime = deadline
    val hasDeadline   = remainingTime>0
    while (waiting) {
      val (feasible, result) = fireFirst(events)
      if (logging) Default.finest(s"$result event retrying ($feasible) $remainingTime")
      // result is OK or all events refused or infeasible
      result match {
        case OK =>
          // an event fired
          outcome = true
          waiting = false
        case CLOSED | NO =>
          // no event seen on this pass fired
          if (feasible == 0) {
            // now nothing CAN happen
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
    // feasibles are the events that might be ready
    if (logging) Default.finer(s"fireFirst $events")
    val feasibles = events.filter{
      case `!!`(guard, port, _) => guard() && port.canWrite
      case `??`(guard, port, _) => guard() && port.canRead
    }
    val iter: Iterator[Event] = feasibles.iterator
    // find and fire the first ready event
    while (outcome!=OK && iter.hasNext) {
      iter.next() match {
        case `!!`(guard, port, f) =>
            outcome = port.offer(f)
            outcome match {
              case OK => feasible += 1
              case _  =>
            }
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
      for { in<-ins } in.closeIn()
      out.closeOut()
      Default.finer(s"merge($ins)($out) terminated after closing")
  }

  def source[T](out: OutPort[T], it: Iterable[T]): proc = proc (s"source($out,...)"){
      var count = 0
      RuntimeDatabase.newLocal("count", count)
      repeatFor (it) { t => out!t; count += 1 }
      out.closeOut()
      Default.finer(s"source($out) closed")
  }

  def sink[T](in: InPort[T])(andThen: T=>Unit): proc = proc (s"sink($in)"){
      var count = 0
      RuntimeDatabase.newLocal("count", count)
      repeatedly { in?{ t => andThen(t); count+=1 } }
      in.closeIn()
      Default.finer(s"sink($in) closed")
  }
}

