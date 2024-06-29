package org.sufrin.glyph

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/**
 * A ultra-lightweight scheduler implementing the (possibly-repeated)
 * delayed execution of an event. Every schedule is associated with an
 * event and an interval (calibrated in milliseconds).
 *
 * Events, when executed, run on a virtual (lightweight) thread  associated with the schedule;
 * and delays are implemented as sleeps in that thread.
 *
 * If the interval is zero, then it is taken to be indefinite. Events awaiting such intervals
 * can be `cancel`led, or invoked `immediately()`
 */
class Schedule() {
  /** The thread currently awaiting (or executing) an event */
  final private
  val thread: AtomicReference[Thread] = new AtomicReference[Thread](null)
  /** The interval between successive event executions */
  final private
  val theInterval: AtomicLong         = new AtomicLong(0)
  final private
  var runEvent: () => Unit            = { ()=>assert(false, "Schedule has no associated event")}

  /**
   * Cancel the currently-awaited event, if any; and cancel
   * it forever if it's running periodically.
   */
  def cancel(): Unit = synchronized {
    val th = thread.getAndSet(null)
    if (th ne null) { th.interrupt() }
  }

  /**
   * Run the currently-awaited event, if any, immediately -- without
   * cancelling it if it's running periodically.
   */
  def immediately(): Unit = synchronized {
    val th = thread.get()
    if (th ne null) { th.interrupt() }
  }

  /**
   * Cancel the currently-awaited event, if any. Associate the
   * `interval` and `event` with this scheduler, and execute the
   * event once after the interval.
   *
   * @param interval
   * @param event
   */
  def once(interval: Long)(event: => Unit): Unit = synchronized {
    theInterval.set(interval)
    runEvent = { () => event }
    once()
  }

  /**
   * Associate the interval and the event with `interval`, and `event` without actually scheduling anything.
   *
   * Requires: `!running`
   *
   * @param interval
   * @param event
   *
   * @see once
   * @see periodically
   */
  def apply(interval: Long)(event: => Unit): Unit = synchronized {
    assert(!running, s"aSchedule($interval)(...) can only be called when aSchedule is not running")
    theInterval.set(interval)
    runEvent = { () => event }
  }

  /**
   * Cancel the currently-scheduled event, if any. Schedule one execution of
   * the associated event to take place after the associated interval.
   */
  def once(): Unit = {
    Thread.startVirtualThread {
      () =>
        val current = thread.getAndSet(Thread.currentThread())
        if (current ne null) {
          current.interrupt()
        }
        else {
          try Thread.sleep(theInterval.get) catch {
            case _: InterruptedException =>
          } finally {
            if (thread.getAndSet(null) ne null) {
              runEvent()
            }
          }
        }
    }
  }


  /**
   * Cancel the currently-scheduled event, if any. Associate
   * `interval` and `event` with this scheduler, and execute the
   * associated event periodically every associated interval.
   *
   * @param interval
   * @param event
   */
  def periodically(interval: Long)(event: => Unit): Unit = synchronized {
    theInterval.set(interval)
    runEvent = { () => event }
    periodically()
  }

  /**
   * Cancel the currently-scheduled event, if any; then execute the
   * associated event periodically every associated interval.
   */
  def periodically(): Unit = {
    Thread.startVirtualThread {
      () =>
        val current = thread.getAndSet(Thread.currentThread())
        var running = true
        if (current ne null) {
          current.interrupt()
        }
        else {
          while (running) {
            try Thread.sleep(theInterval.get()) catch {
              case _: InterruptedException =>
                running = false
            }
            if (thread.get() ne null) runEvent()
            if (!running) thread.set(null)
          }
        }
    }
  }

  /**
   * Revise the associated interval. It will come into force after
   * the currently-associated interval elapses.
   * @param interval
   */
  def period_=(interval: Long): Unit = theInterval.set(interval)
  /** The associated interval */
  def period: Long                   = theInterval.get

  /**
   * True if the scheduler is waiting to run an event, or is running an
   * event periodically.
   */
  def running: Boolean = thread.get() ne null
}

object Schedule {
  def apply(): Schedule = new Schedule()
  def apply(interval: Long)(event: => Unit): Schedule = {
    val schedule = new Schedule()
    schedule(interval)(event)
    schedule
  }
}