import scala.annotation._

object p04 extends App {

  /*
    PROBLEM 04

    (*) Find the number of elements of a list.
    Example:
      scala> length(List(1, 1, 2, 3, 5, 8))
      res0: Int = 6

  */

  val myList = List(1, 1, 2, 3, 5, 8)

  def length[A](l: List[A]): Int = {
    @tailrec
    def lengthHelper(l: List[A], accumulator: Int): Int = {
      if (l.isEmpty) accumulator
      else lengthHelper(l.tail, accumulator + 1)
    }

    lengthHelper(l, 0)
  }

  def lengthWithCase[A](l: List[A]): Int = l match {
    case Nil => 0
    case _ :: tail => 1 + lengthWithCase(tail)
  }

  def lengthWithCaseTailRecursive[A](l: List[A]): Int = {
    @tailrec
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }

    lengthR(0, l)
  }

  def lengthFunctional[A](l: List[A]): Int = l.foldLeft(0) { (c, _) => c + 1 }

  println(length(myList))
  println(lengthWithCase(myList))
  println(lengthWithCaseTailRecursive(myList))
  println(lengthFunctional(myList))

}
