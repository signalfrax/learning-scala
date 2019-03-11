/**
  * Implement the Bag algorithm.
  *
  * The Bag will be a recursive data structure
  */
object BagAlgorithm extends App{

  sealed trait Bag[+T] {
    def add[B >: T](item: B): Bag[B]
    def isEmpty: Boolean
    def size: Int
    def foreach(f: T => Unit): Unit
  }

  object Bag {
    def apply[T]: Bag[T] = EmptyBag
  }

  final case class BagItem[+T](item: T, bag: Bag[T]) extends Bag[T] {

    override def add[B >: T](item: B): Bag[B] = BagItem(item, this)
    override def isEmpty: Boolean = 0 == size
    override def size: Int = size + 1
    override def foreach(f: T => Unit): Unit = {
      f(item)
      bag.foreach(f)
    }

  }

  final case object EmptyBag extends Bag[Nothing] {

    override def add[B >: Nothing](t: B): Bag[B] = BagItem(t, this)
    override def isEmpty: Boolean = true
    override def size: Int = 0
    override def foreach(f: Nothing => Unit): Unit = ()
  }

  val b = Bag[String]
  val c = b
    .add("hammer")
    .add("saw")
    .add("screwdriver")
    .add("drill")

  c.foreach( s => println(s) )

}
