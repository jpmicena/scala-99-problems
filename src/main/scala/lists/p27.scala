package lists


object p27 extends App {

  /*
    P27 (**) Group the elements of a set into disjoint subsets.
     a) In how many ways can a group of 9 people work in 3 disjoint subgroups
        of 2, 3 and 4 persons?  Write a function that generates all the
        possibilities.

        Example:
        scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

     b) Generalize the above predicate in a way that we can specify a list
        of group sizes and the predicate will return a list of groups.

        Example:
        scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip" "Gary", "Hugo", "Ida"))
        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...

     Note that we do not want permutations of the group members;
     i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
     However, we make a difference between ((Aldo, Beat), (Carla, David), ...)
     and ((Carla, David), (Aldo, Beat), ...).

     You may find more about this combinatorial problem in a good book on
     discrete mathematics under the term "multinomial coefficients".

   */

  val myList = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")

  def flatMapSublists[A, B](l: List[A])(f: (List[A]) => List[B]): List[B] =
    l match {
      case Nil                   => Nil
      case sublist @ _ :: tail => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, l: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else
      flatMapSublists(l) { sl =>
        combinations(n - 1, sl.tail) map { sl.head :: _ }
      }

  def group3[A](l: List[A]): List[List[List[A]]] = for {
    a <- combinations(2, l)
    noA = l.filterNot(a.contains(_))
    b <- combinations(3, noA)
  } yield List(a, b, noA.filterNot(b.contains(_)))

  def group[A](groupings: List[Int], l: List[A]): List[List[List[A]]] =
    if (groupings.sum > l.length) throw new RuntimeException("More elements than list length")
    else groupings match {
      case Nil => List(Nil)
      case n :: ns => for {
        a <- combinations(n, l)
        b <- group(ns, l.filterNot(a.contains(_)))
        c = a :: b
      } yield c
  }

  group3(myList).foreach(println)
  group(List(3, 2, 4), myList).foreach(println)
}


