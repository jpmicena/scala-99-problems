package lists

import scala.annotation.tailrec

object p01 extends App {

  /*
      PROBLEM 01

      (*) Find the last element of a list.
      Example:
        scala> last(List(1, 1, 2, 3, 5, 8))
        res0: Int = 8

   */

  val myList: List[Int] = List(1, 1, 2, 3, 5, 8)

  def last[A](l: List[A]): A = {
    @tailrec
    def lastHelper(remaning: List[A]): A =
      if (remaning.tail.isEmpty) remaning.head
      else lastHelper(remaning.tail)

    lastHelper(l)
  }

  def lastBuiltin[A](l: List[A]): A = l.last

  def lastWithCase[A](l: List[A]): A = l match {
    case h :: Nil => h
    case _ :: tail => lastWithCase(tail)
    case _ => throw new NoSuchElementException
  }

  println(last(myList))
  println(lastBuiltin(myList))
  println(lastWithCase(myList))

}
