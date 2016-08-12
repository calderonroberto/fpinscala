package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }
  }

  // TODO: discuss
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Nil, _) => Nil
      case (Cons(_, t), 1) => t
      case (Cons(_, t), _) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil // remove last element
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((l, n) => n + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object Chapter2 {
  def main(args: Array[String]): Unit = {
    val List = fpinscala.datastructures.List
    println(List(1, 2, 3))

    println("> Tail")
    println(List.tail(List(1, 2, 3, 4)))
    println(List.tail(List()))

    println("> SetHead")
    println(List.setHead(List(2, 3, 4), 6))
    println(List.setHead(List(), 1))

    println("> Drop")
    println(List.drop(List(1, 2, 3, 4), 2))
    println(List.drop(List(), 5))

    println("> DropWhile")
    println(List.dropWhile(List(4, 2, 1, 8), (x: Int) => x > 1))

    println("> Init")
    println(List.init(List(1, 2, 3, 4, 5)))

    println("> foldRight")
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

    println("> length")
    println(List.length(List(0, 1, 3, 4, 5)))

    println("> FoldLeft")
    println(List.foldLeft(List(4,2,3), 0)(_ + _))

  }
}