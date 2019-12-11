package lists

object p14 extends App {

  /*
    PROBLEM 14

     (*) Duplicate the elements of a list.

      Example:
        scala> duplicate(List('a, 'b, 'c, 'c, 'd))
        res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  */

  val myList = List("a", "b", "c", "c", "d")

  def duplicate[A](l: List[A]): List[A] = l flatMap {
    List.fill(2)(_)
  }

  println(duplicate(myList))

}
