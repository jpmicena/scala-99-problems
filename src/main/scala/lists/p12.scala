package lists

object p12 extends App {

  /*

    (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

    Example:
      scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  */

  val myList = List((4, "a"), (1, "b"), (2, "c"), (2, "a"), (1, "d"), (4, "e"))

  def decode[A](l: List[(Int, A)]): List[A] = l.iterator.flatMap( x =>
    List.fill(x._1)(x._2)
  ).toList

  println(decode(myList))

}
