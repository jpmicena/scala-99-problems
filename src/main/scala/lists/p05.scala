package lists

object p05 extends App {

  /*
    PROBLEM 05

    (*) Reverse a list
    Example:
      scala> reverse(List(1, 1, 2, 3, 5, 8))
      res0: List[Int] = List(8, 5, 3, 2, 1, 1)

  */

  val myList = List(1, 1, 2, 3, 5, 8)

  def reverse[A](l: List[A]): List[A] =
    l.foldLeft(List.empty[A]) { (reversed, e) => e +: reversed }

  def reverseWithCase[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail => reverseWithCase(tail) :+ h
  }

  println(reverse(myList))
  println(reverseWithCase(myList))
}
