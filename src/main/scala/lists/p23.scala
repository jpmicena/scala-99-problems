package lists

object p23 extends App {

  /*
  PROBLEM 23

    (**) Extract a given number of randomly selected elements from a list.

    Example:
      scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
      res0: List[Symbol] = List('e, 'd, 'a)

    Hint: Use the solution to problem P20

 */

  val myList = List("a", "b", "c", "d", "e", "f", "g", "h")

  def removeAt[A](n: Int, l: List[A]): (List[A], A) = l.splitAt(n) match {
    case (_, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (_, Nil)        => throw new NoSuchElementException
  }

  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    val nRemove = l.length - n
    if (nRemove < 0) throw new NoSuchElementException
    else
      List.range(0, nRemove).foldLeft(l) { (curL, _) =>
        val r = scala.util.Random
        removeAt(r.nextInt(curL.length), curL)._1
      }
  }

  def randomSelectRecursive[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelectRecursive(n - 1, rest)
    }

  def randomSelectRecursive2[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }

  println(randomSelect(3, myList))

}
