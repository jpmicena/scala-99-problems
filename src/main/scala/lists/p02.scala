package lists

import scala.annotation.tailrec

object p02 extends App {

  /*
    PROBLEM 02

    (*) Find the last but one element of a list.
    Example:
      scala> penultimate(List(1, 1, 2, 3, 5, 8))
      res0: Int = 5

 */

  val myList: List[Int] = List(1, 1, 2, 3, 5, 8)

  @tailrec
  def penultimate[A](l: List[A]): A = l match {
    case h :: _ :: Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimateWithoutCase[A](l: List[A]): A = {
    @tailrec
    def penultimateHelper(remaining: List[A]): A = {
      if (remaining.length < 2) throw new NoSuchElementException
      else if (remaining.length == 2) remaining.head
      else penultimateHelper(remaining.tail)
    }

    penultimateHelper(l)
  }

  def penultimateBuiltin[A](l: List[A]): A = l.init.last

  println(penultimate(myList))
  println(penultimateWithoutCase(myList))
  println(penultimateBuiltin(myList))

}
