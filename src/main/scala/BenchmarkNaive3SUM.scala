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

  val numbers = generateNumbers(250)
  val timer = Stopwatch()
  val count = Naive3SUM.count(numbers)
  val duration = timer.elapsedTime

  println(s"Found $count in $duration")


}
