package lists

object p11 extends App {

  /*
    PROBLEM 11

     (*) Modified run-length encoding.
      Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
      Only elements with duplicates are transferred as (N, E) terms.

      Example:
        scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
        res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

  */

  val myList = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")

  def encodeModified[A](l: List[A]): List[Any] =
    p10.encode(l) map { t => if (t._1 == 1) t._2 else t }


  def encodeModifiedTypeSafe[A](l: List[A]): List[Either[A, (Int, A)]] =
    p10.encode(l) map { t => if (t._1 == 1) Left(t._2) else Right(t) }

  println(encodeModified(myList))

}
