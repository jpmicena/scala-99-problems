package lists

import scala.annotation.tailrec

object p20 extends App {

  /*
    PROBLEM 20

    (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.

    Example:
      scala> removeAt(1, List('a, 'b, 'c, 'd))
      res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

   */

  val myList = List("a", "b", "c", "d")

  def removeAt[A](n: Int, l: List[A]): (List[A], A) = {

    @tailrec
    def removeAtHelper(target: Int, curN: Int, curL: List[A], result: List[A]): (List[A], A) = {
      if (curN == target) (result ::: curL.tail, curL.head)
      else removeAtHelper(target, curN + 1, curL.tail, result :+ curL.head)
    }

    removeAtHelper(n, 0, l, Nil)
  }

  def removeAtAlternative[A](n: Int, l: List[A]): (List[A], A) = l.splitAt(n) match {
    case (_, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (_, Nil)        => throw new NoSuchElementException
  }

  println(removeAt(1, myList))
  println(removeAtAlternative(1, myList))
}


