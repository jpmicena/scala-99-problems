package lists

object p06 extends App {

  /*
    PROBLEM 05

    (*) Find out whether a list is a palindrome.
    Example:
      scala> isPalindrome(List(1, 2, 3, 2, 1))
      res0: Boolean = true

  */

  val myList = List(1, 2, 3, 2, 1)

  def isPalindrome[A](l: List[A]): Boolean =
    l == p05.reverse(myList)

  println(isPalindrome(myList))

}
