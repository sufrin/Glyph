package org.sufrin.glyph

import java.util.concurrent.atomic.AtomicReference

/**
 * Scheduler for single, cancellable, events
 */
class Schedule() {
  val thread: AtomicReference[Thread] = new AtomicReference[Thread](null)

  /**
   * Cancel the scheduled event, if any.
   */
  def cancel(): Unit = synchronized {
    val th = thread.getAndSet(null)
    if (th ne null) { th.interrupt() }
  }

  /**
   * Run the scheduled event, if any, immediately
   */
  def now(): Unit = synchronized {
    val th = thread.get()
    if (th ne null) { th.interrupt() }
  }

  /**
   * Cancel the currently-scheduled event, if any; then schedule the execution of `event` after `delta` milliseconds.
   *
   * @param delta
   * @param event
   */
  def apply(delta: Long)(event: => Unit): Unit = synchronized {
    Thread.startVirtualThread {
      () =>
        val current = thread.getAndSet(Thread.currentThread())
        if (current ne null) {
          current.interrupt()
        }
        else {
          try Thread.sleep(delta) catch {
            case _: InterruptedException =>
          } finally {
            if (thread.getAndSet(null) ne null) {
              event
            }
          }
        }
    }
  }
}