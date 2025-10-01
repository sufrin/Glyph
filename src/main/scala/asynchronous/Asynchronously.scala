package org.sufrin.glyph
package asynchronous

/** Evaluate expressions (and commands) in a background thread */
object Asynchronously {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  var defaultTimeout: Duration = 20.seconds

  def apply[T](expr: => T): T = Await.result(Future(expr), defaultTimeout)

  def apply[T](timeout: Duration) (expr: => T): T = Await.result(Future(expr), timeout)

  def run(expr: => Unit): Unit = {
    Await.result(Future({ expr; true }), 20.seconds)
    ()
  }

  def run(timeout: Duration) (expr: => Unit): Unit = {
    Await.result(Future({ expr; true }), timeout)
    ()
  }
}

