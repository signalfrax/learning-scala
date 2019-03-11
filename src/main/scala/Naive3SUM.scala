import scala.annotation.tailrec

/**
  * This is a naive implementation of the 3SUM algorithm.
  */
object Naive3SUM extends App {

  def count(l: List[Int]): Int = {

    @tailrec
    def countHelper(l: List[Int], i: Int, j: Int, k: Int, acc: Int): Int = {

      if (i > l.length) acc
      else if (j > l.length) {
        val ni = i + 1
        val nj = ni + 1
        val nk = nj + 1
        countHelper(l, i + 1, nj, nk, acc)
      }
      else if (k > l.length) {
        val nj = j + 1
        val nk = nj + 1
        countHelper(l, i, nj, nk, acc)
      }
      else {
        val count = if (0 == l(i - 1) + l(j - 1) + l(k - 1)) acc + 1 else acc
        println(s"$i $j $k | ${l(i - 1)} ${l(j - 1)} ${l(k - 1)} | $count")
        countHelper(l, i, j, k + 1, count)
      }

    }

    if (l.length < 2) 0 else countHelper(l, 1, 2, 3, 0)

  }

  val numbers = List(1,2,3,-1,-2,-3,5)
  println(count(numbers))

}
