import scala.annotation.tailrec

/**
  * Implement binary search in an immutable way using tail recursion.
  */
object BinarySearch extends App {

  def binarySearch(a: List[Int], k: Int): Option[Int] = {

    @tailrec
    def search(a: List[Int], k: Int, lo: Int, hi: Int) : Option[Int] = {
      val mid = lo + (hi - lo) / 2
      if(lo > hi) None
      else if (k < a(mid)) search(a, k, lo, mid - 1)
      else if (k > a(mid)) search(a, k, mid + 1, hi)
      else Some(mid)
    }

    search(a, k, 0, a.length - 1)

  }

  val numbers = List[Int](1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

  for {
    idx <- binarySearch(numbers, 10)
  } println(idx, numbers(idx))

}
