/**
  * Implement LIFO Stack using a recursive data structure as the underlying storage.
  */
object StackLinkedListImpl extends App {

  sealed trait Node[+T] {
    def size = 0
  }
  final case class ItemNode[+T](value: T, next: Node[T] = EmptyNode, override val size: Int) extends Node[T]
  final case object EmptyNode extends Node[Nothing]

  case class Stack[+T](node: Node[T] = EmptyNode) {
    def push[B >: T](value: B): Stack[B] = Stack[B](ItemNode(value, node, node.size + 1))
    def pop: Option[(T, Stack[T])] =
      node match {
        case ItemNode(v, n, _) => Some((v, Stack[T](n)))
        case EmptyNode => None
      }
    def size: Int = node.size
    def isEmpty: Boolean = 0 == node.size
    def foreach(f: T => Unit): Unit =
      for {
        (v, n) <- pop
      } yield (f(v), n.foreach(f))

  }


  val s = Stack[String]()
  val s1 = s.push("1")
  val s2 = s1.push("2")
  val s3 = s2.push("3")

  s3.foreach(s => println(s"Got: $s"))

}
