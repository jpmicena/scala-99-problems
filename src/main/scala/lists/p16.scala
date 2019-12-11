package lists

object p16 extends App {

  /*
    PROBLEM 16

     (**) Drop every Nth element from a list.

      Example:
      scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

  */

  val myList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")

  def drop[A](n: Int, l: List[A]): List[A] = l.grouped(n).flatMap {
    _.slice(0, n - 1)
  }.toList

  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  println(drop(3, myList))
  println(dropFunctional(3, myList))

}
