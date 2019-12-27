package lists

import scala.annotation.tailrec

object p21 extends App {

  /*
    PROBLEM 21

    (*) Insert an element at a given position into a list.

    Example:
      scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
      res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

   */

  val myList = List("a", "b", "c", "d")

  def insertAt[A](e: A, n: Int, l: List[A]): List[A] = {
    @tailrec
    def insertAtHelper(e: A, target: Int, curL: List[A], curN: Int, result: List[A]): List[A] = {
      if (curN == target) (result :+ e) ::: curL
      else insertAtHelper(e, target, curL.tail, curN + 1, result :+ curL.head)
    }
    if (n < 0 || n > (l.length) || l.isEmpty) throw new NoSuchElementException
    else insertAtHelper(e, n, l, 0, Nil)
  }

  def insertAtAlternative[A](e: A, n: Int, l: List[A]): List[A] = l.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
  }

  println(insertAt("new", 1, myList))
  println(insertAtAlternative("new", 1, myList))


}
