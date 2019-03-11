/**
  * The basic stack implemented using List as the underlying storage.
  */
object StackImpl extends App{

  case class Stack[+A](l:List[A] = List()) {
    def push[B >: A](a: B): Stack[B] = Stack( l :+ a )
    def pop: Option[(A, Stack[A])] =
      if (l.length > 1) Some((l.last, Stack(l.slice(0, l.length - 1))))
      else if (l.length == 1) Some((l.last, Stack(List())))
      else None
    def size: Int = l.length
    def isEmpty: Boolean = l.isEmpty
    def map[B >: A](f: A => B): Stack[B] = Stack[B](l.map(f))
    def foreach(f: A => Unit): Unit = l.foreach(f)

  }

  val s = Stack()
  val s1 = s.push("bogus")
  val s2 = s1.push("test")
  val s3 = s2.push("stacker")


  val s3Mod = s3.map( (s:String) => s"$s modified" )
  s3Mod.foreach(s => println(s))


}
