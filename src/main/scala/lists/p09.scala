package lists

object p09 extends App {

  /*
    PROBLEM 09

     (**) Pack consecutive duplicates of list elements into sublists.
      If a list contains repeated elements they should be placed in separate sublists.
      Example:
        scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  */

  val myList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")

  /*
    I understood the problem incorrectly the first time so I came up with this solution, The problem is that
    This function achieves the first line of the problem but not the second one.
   */
  
  def packIncorrect[A](l: List[A]): List[List[A]] = l.distinct.foldLeft(List.empty[List[A]]) { (ls, e) =>
    ls :+ l.filter(x => x == e)
  }

  def packRecursive[A](l: List[A]): List[List[A]] = {
    if (l.isEmpty) List(List())
    else {
      val (packed, next) = l span { _ == l.head }
      if (next == Nil) List(packed)
      else packed :: packRecursive(next)
    }
  }

  println(packIncorrect(myList))

  println(packRecursive(myList))

}
