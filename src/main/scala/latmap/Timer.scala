package latmap

class Timer {
  var stopped = true
  var startTime = 0L
  var elapsedTime = 0L
  def start(): Unit = {
    assert(stopped)
    stopped = false
    startTime = System.nanoTime()
  }
  def stop(): Unit = {
    elapsedTime += System.nanoTime() - startTime
    assert(!stopped)
    stopped = true
  }
}
