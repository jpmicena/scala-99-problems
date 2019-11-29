package lists

object p07 extends App {

  /*
    PROBLEM 07

    (**) Flatten a nested list structure.
    Example:
      scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
      res0: List[Any] = List(1, 1, 2, 3, 5, 8)

  */

  val myList = List(List(1, 1), 2, List(3, List(5, 8)))

  def flatten(l: Any): List[Any] = l match {
    case Nil => Nil
    case h :: tail if h.isInstanceOf[List[Any]] => flatten(h) ::: flatten(tail)
    case h :: tail => h :: flatten(tail)
    case _ => List(l)
  }

  def flattenBetter(l: List[Any]): List[Any] = l flatMap {
    case ms: List[_] => flattenBetter(ms)
    case e => List(e)
  }

  println(flatten(myList))
  println(flatten(2))
  println(flattenBetter(myList))
}
