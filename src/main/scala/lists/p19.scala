package lists

import scala.annotation.tailrec

object p19 extends App {

  /*
    PROBLEM 19

     (**) Rotate a list N places to the left.
      Examples:

        scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

        scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
        res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

  */

  val myList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")

  def rotate[A](n: Int, l: List[A]): List[A] = {
    @tailrec
    def rotateHelper(target: Int, curN: Int, result: List[A]): List[A] =
      if (curN < target) rotateHelper(target, curN + 1, result.tail :+ result.head)
      else if (curN > target) rotateHelper(target, curN - 1, result.last +: result.init)
      else result

    rotateHelper(n, 0, l)
  }

  @tailrec
  def rotateAlternative[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotateAlternative(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  println(rotate(3, myList))
  println(rotate(-2, myList))

  println(rotateAlternative(3, myList))
  println(rotateAlternative(-2, myList))

}
