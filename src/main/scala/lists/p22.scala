package lists

import scala.annotation.tailrec

object p22 extends App {

  /*
    PROBLEM 22

    (*) Create a list containing all integers within a given range.

    Example:
      scala> range(4, 9)
      res0: List[Int] = List(4, 5, 6, 7, 8, 9)

   */

  def range(start: Int, end: Int): List[Int] = {
    @tailrec
    def rangeHelper(curN: Int, end: Int, result: List[Int]): List[Int] = {
      if (curN == end) result :+ curN
      else rangeHelper(curN + 1, end, result :+ curN)
    }
    rangeHelper(start, end, Nil)
  }

  def rangeRecursive(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: rangeRecursive(start + 1, end)

  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None         => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }
  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n, n + 1))
    }

  println(range(4, 9))
  println(rangeFunctional(4, 9))



}
