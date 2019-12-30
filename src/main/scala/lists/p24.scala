package lists

object p24 extends App {

  /*
    PROBLEM 24

    (*) Lotto: Draw N different random numbers from the set 1..M.

    Example:
      scala> lotto(6, 49)
      res0: List[Int] = List(23, 1, 17, 33, 21, 37)

   */

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

  def lotto(n: Int, end: Int): List[Int] = {
    randomSelect(n, List.range(1, end + 1))
  }

  println(lotto(6, 49))
  println(lotto(6, 49))
  println(lotto(6, 49))
  println(lotto(6, 49))

}
