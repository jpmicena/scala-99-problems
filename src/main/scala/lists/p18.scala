package lists

import scala.annotation.tailrec

object p18 extends App {

  /*
    PROBLEM 18

    (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

    Example:
      scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('d, 'e, 'f, 'g)

  */

  val myList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")

  def slice[A](i: Int, k: Int, l: List[A]): List[A] = {
    @tailrec
    def sliceHelper(curL: List[A], curI: Int, curK: Int, result: List[A]): List[A] =
      (curL, curI, curK) match {
        case (Nil, _, _)         => result
        case(_, 0, 0)            => result
        case (h :: tail, 0, n)   => sliceHelper(tail, 0, n - 1, result :+ h)
        case(_ :: tail, n, m)    => sliceHelper(tail, n - 1, m - 1, result)
    }
    sliceHelper(l, i, k, Nil)
  }

  def sliceWithGuards[A](i: Int, k: Int, l: List[A]): List[A] = {
    @tailrec
    def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
      (count, curList) match {
        case (_, Nil)                     => result
        case (c, _)           if k <= c   => result
        case (c, h :: tail)   if i <= c   => sliceR(c + 1, tail, result :+ h)
        case (c, _ :: tail)               => sliceR(c + 1, tail, result)
      }
    sliceR(0, l, Nil)
  }

  println(slice(3, 7, myList))
  println(sliceWithGuards(3, 7, myList))

}
