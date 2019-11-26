package lists

import scala.annotation.tailrec

object p03 extends App {

  /*
    PROBLEM 03

    (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:
      scala> nth(2, List(1, 1, 2, 3, 5, 8))
       res0: Int = 2

  */

  val myList: List[Int] = List(1, 1, 2, 3, 5, 8)

  @tailrec
  def nth[A](n: Int, l: List[A]): A = (n, l) match {
    case (0, h :: _)    => h
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil)       => throw new NoSuchElementException
  }

  println(nth(2, myList))

}
