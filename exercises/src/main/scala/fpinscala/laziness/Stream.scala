package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  final def toListTailRec: List[A] = { //had a peeek at the solution, because i didn't get why it was not compiling
    @annotation.tailrec
    def makeList(l: List[A], s: Stream[A]): List[A] = s match {
        case Cons(h, t) => makeList(h() :: l, t())
        case _ => l
      }
    makeList(List(), this).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =   //TODO: I don't get this explanation, how does 'b' fold the tail? how does scala know to run fold on it?
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match  {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().drop(n-1))
    case _ => empty
  }

  @annotation.tailrec
  final def dropTailRec(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().dropTailRec(n - 1)
    case _ => this
  }

  final def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p)) // meh, forgot to  create a new cons(), instead i was passing only the tail
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h,t) => p(h) && t)

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty[A] ) // TODO: i kind of get it, but i'm still missing why cons(h,t), 't' will call fold right

  def headOption: Option[A] = foldRight(None: Option[A])((h,t) => Some(h)) //TODO:  is Empty[A] == None: Option[A]?

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t)) //TODO: is empty[B] == Empty: Stream[B]?

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  //append adds the tail to
  def append[B>:A](f: => Stream[B]): Stream[B] = foldRight(f)((h,t) => cons(h,t)) //TODO: seriously, how does t call append again?

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fib: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(a, a+b))
    }
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,t)) => cons(h, unfold(t)(f)) // this ((a,b)) threw me off, it's ONE parameter, not two
      case _ => empty
    }
  }

  def fibUnfold: Stream[Int] = unfold((0,1)){case(a,b) => Some(a,(b,a+b))} //meh, had to peek
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))
  def onesUnfold= unfold(1)(_ => Some(1,1))

  def mapUnfold[B](f: A=>B): Stream[B] = unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }
  def takeUnfold(n: Int): Stream[A] = unfold(this, n){
    case (Cons(h,t), 1) => Some(h(), (empty,0))
    case (Cons(h,t), n) if n>1 => Some(h(), (t(), n-1))
    case _ => None
  }

  def takeWhileUnfold(p: A=> Boolean): Stream[A] =
    unfold(this){
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this, s2){
      case (Cons(h,t), Cons(h2,t2)) =>
        Some((f(h(), h2()), (t(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_, _))

  //this was hard and had to look at the answer. :(
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s) takeWhile {
    case (_, b) => b.isDefined
  } forAll{
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] =
  unfold(this){
    case Empty => None
    case s => Some((s, s.drop(1)))
  } append Stream(empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object Chapter5 {
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3)
    println(s.toListTailRec)
    println(s.constant(1).take(4).toList)
    println(s.from(100).toList)
  }
}
