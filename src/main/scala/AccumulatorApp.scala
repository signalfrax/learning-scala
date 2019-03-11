/**
  * Implement a simple accumulator.
  *
  * Basically accumulates values and gives back the mean.
  */
object AccumulatorApp extends App {


  case class Accumulator(sum: Double, n: Int) {
    def mean(): Double = sum / n
    def addDataValue(v: Double): Accumulator = Accumulator(sum + v, n)

    override def toString: String = s"Mean ($n values): " + s"${mean()}"
  }

  val m = Accumulator(10, 1)
  val m1 = m.addDataValue(20)

  println(m1)



}
