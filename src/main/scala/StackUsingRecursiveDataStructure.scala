/**
  * Implement LIFO Stack with the stack itself being the recursive data structure.
  */
object StackUsingRecursiveDataStructure extends App {

  sealed trait Stack[+T] {

    def size: Int
    def push[B >: T](value: B): Stack[B]
    def pop:Option[(T, Stack[T])]
    def isEmpty: Boolean = 0 == size
    def foreach(f: T => Unit): Unit
    def map[B >: T](f: T => B): Stack[B]

    // Add stacks together.
    def ++[B >: T](stack: Stack[B]): Stack[B]
    def filter[B >: T](f: T => Boolean): Stack[B]

  }

  object PushdownStack {
    def apply[T]: Stack[T] = PushdownStackEmpty
  }

  final case class PushdownStack[+T](value: T, stack: Stack[T]) extends Stack[T] {

    override def size: Int = stack.size + 1
    override def push[B >: T](value: B): Stack[B] = PushdownStack[B](value, this)
    override def pop: Option[(T, Stack[T])] = Some(value, stack)
    override def foreach(f: T => Unit): Unit = {
      f(value)
      stack.foreach(f)
    }
    override def map[B >: T](f: T => B): Stack[B] = PushdownStack[B](f(value), stack.map(f))

    /**
      * This is an incorrect implementation.
      *
      * Say we have the following stacks:
      *   stack A with items (4,3,2,1)
      *   stack B with items (6,5)
      *
      * Combining these stacks together should give us stack C with items (6,5,4,3,2,1)
      *
      * But since we are popping items from stack B and pushing them on stack A we get
      *
      * (5,6,4,3,2,1) which is incorrect.
      *
      * I haven't figured out how to implement an immutable FIFO data structure yet.
      *
      */
    override def ++[B >: T](s: Stack[B]): Stack[B] =
      s.pop match {
        case None => this
        case Some((nextValue, nextStack)) => push(nextValue) ++ nextStack
      }

    override def filter[B >: T](f: T => Boolean): Stack[B] =
      if (f(value)) PushdownStack[B](value, stack.filter(f)) else stack.filter(f)


  }

  final case object PushdownStackEmpty extends Stack[Nothing] {

    override def size: Int = 0
    override def push[B >: Nothing](value: B): Stack[B] = PushdownStack[B](value, this)
    override def pop: Option[(Nothing, Stack[Nothing])] = None
    override def foreach(f: Nothing => Unit): Unit = Unit
    override def map[B >: Nothing](f: Nothing => B): Stack[B] = PushdownStackEmpty
    override def ++[B >: Nothing](stack: Stack[B]): Stack[B] = stack
    override def filter[B >: Nothing](f: Nothing => Boolean): Stack[B] = PushdownStackEmpty

  }

  val ps = PushdownStack[Int]
  val ps1 = ps.push(1)
  val ps2 = ps1.push(2)
  val ps3 = ps2.push(3)
  val ps4 = ps3.push(4)

  // val ps5 = ps4.map( x => x * 2 )

  val pa = PushdownStack[Int]
  val pa1 = pa.push(5)
  val pa2 = pa1.push(6)

  val c = ps4 ++ pa2

  //  c.foreach(s => println(s)) // Actually we should have 6,5,4,3,2,1
  val f = c.filter(i => i >= 2 && i <= 5)
  f.foreach(i => println(i))

}
