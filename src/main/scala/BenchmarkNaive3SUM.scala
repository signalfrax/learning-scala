import scala.annotation.tailrec

/**
  * Let's create a benchmark.
  */
object BenchmarkNaive3SUM extends App {

  def generateNumbers(n: Int): List[Int] = {
    val max = 1000
    val min = -1000

    @tailrec
    def helper(n: Int, l: List[Int] = List()): List[Int] = {
      if (0 == n) l
      else helper(n - 1, l :+ ((Math.random()*((max-min)+1))+min).toInt)
    }
    helper(n)

  }

  case class Stopwatch(start: Long = System.currentTimeMillis()) {
    def elapsedTime: Double = (System.currentTimeMillis() - start) / 1000.0
  }

  @tailrec
  def benchmark(n: Int): Unit = {
    println("Running...")
    val numbers = generateNumbers(n)
    val timer = Stopwatch()
    val count = Naive3SUM.count(numbers)
    val duration = timer.elapsedTime
    println(s"Total numbers ${numbers.length} found $count in $duration")
    benchmark(n * 2)
  }

  benchmark(250)



}
