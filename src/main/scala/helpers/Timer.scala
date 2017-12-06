package helpers

class Timer private (startTime: Long) {
  def stop(): Long = (System.nanoTime - startTime) / 1000000
}

object Timer {
  def start(): Timer = new Timer(System.nanoTime)
}