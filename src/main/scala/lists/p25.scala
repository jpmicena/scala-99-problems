package lists

object p25 extends App {

  /*
    PROBLEM 25

    (*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.

    Example:
      scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
      res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

   */
  val myList = List("a", "b", "c", "d", "e", "f")

  // Problem 23
  def removeAt[A](n: Int, l: List[A]): (List[A], A) = l.splitAt(n) match {
    case (_, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (_, Nil)        => throw new NoSuchElementException
  }

  // Problem 23
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }

  def randomPermute[A](l: List[A]): List[A] = {
    randomSelect(l.length, l)
  }

  println(randomPermute(myList))
}
