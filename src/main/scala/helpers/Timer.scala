package helpers

class Timer private (startTime: Long) {
  def stop(): Long = (System.nanoTime - startTime) / 1000000
}

case class TimerResult[T](result: T, elapsed: Long)

object Timer {
  def start(): Timer = new Timer(System.nanoTime)

  def time[T](f: => T): TimerResult[T] = {
    val t = new Timer(System.nanoTime)
    val result = f
    val elapsed = t.stop()
    TimerResult(result, elapsed)
  }
}
