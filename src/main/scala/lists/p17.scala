package lists

import scala.annotation.tailrec

object p17 extends App {

  /*
    PROBLEM 17

     (*) Split a list into two parts.
      The length of the first part is given. Use a Tuple for your result.

      Example:
        scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  */

  val myList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")

  def split[A](len: Int, ls: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitHelper(ls1: List[A], ls2: List[A], counter: Int): (List[A], List[A]) = {
      if (counter == len) (ls1, ls2)
      else
        splitHelper(ls1 :+ ls2.head, ls2.tail, counter + 1)
    }

    splitHelper(Nil, ls, 0)
  }

  def splitWithCase[A](len: Int, ls: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitHelper(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
        case (_, Nil)       => (pre, Nil)
        case (0, list)      => (pre, list)
        case (n, h :: tail) => splitHelper(n - 1, tail, pre :+ h)
      }
    splitHelper(len, ls, Nil)
  }


  println(split(3, myList))
  println(splitWithCase(3, myList))
}
