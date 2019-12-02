package lists

object p08 extends App {

  /*
    PROBLEM 08

    (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    Example:
      scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  */

  val myList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")

  def compress[A](l: List[A]): List[A] = l.foldLeft(List.empty[A]) { (ls, e) =>
    if (ls.contains(e)) ls
    else ls :+ e
  }

  def compressRecursive[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: tail => h :: compressRecursive(tail.filterNot(_ == h))
  }

  println(compress(myList))
  println(compressRecursive(myList))

}
