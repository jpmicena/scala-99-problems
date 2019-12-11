package lists

object p15 extends App {

  /*
    PROBLEM 15

     (**) Duplicate the elements of a list a given number of times.

      Example:
        scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
        res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

  */

  val myList = List("a", "b", "c", "c", "d")

  def duplicateN[A](n: Int, l: List[A]): List[A] = l flatMap {
    List.fill(n)(_)
  }

  println(duplicateN(3, myList))

}
