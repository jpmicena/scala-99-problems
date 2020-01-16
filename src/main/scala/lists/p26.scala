package lists

import java.time.Instant

object p26 extends App {

  /*
  PROBLEM 25

    (**) Generate the combinations of K distinct objects chosen from the N elements of a list.

    In how many ways can a committee of 3 be chosen from a group of 12 people?
    We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
    For pure mathematicians, this result may be great. But we want to really generate all the possibilities.

    Example:
      scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
      res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

   */

  val myList = List("a", "b", "c", "d", "e", "f")

  def flatMapSublists[A, B](l: List[A])(f: (List[A]) => List[B]): List[B] =
    l match {
      case Nil                   => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, l: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else
      flatMapSublists(l) { sl =>
        combinations(n - 1, sl.tail) map { sl.head :: _ }
      }

  println(combinations(3, myList).length)

  val t: Instant = Instant.now()

}
