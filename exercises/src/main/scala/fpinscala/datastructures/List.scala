package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("the list is empty")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if (n <= 0) => Cons(x, xs)
    case Cons(_, xs) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case xs => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Cons(y, xs)) => Cons(x, init(Cons(y, xs)))
    case xs => Nil
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => 1 + y)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]) : List[A] =
    foldLeft(l, Nil:List[A]) ((xs, x) => Cons(x, xs))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b:B, a:A) => f(a, b))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a: A, b: B) => f(b, a))

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) ((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def addOneToEach(l: List[Int]) : List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOneToEach(xs))
  }

  def fromDoubleToStr(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString(), fromDoubleToStr(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def removeOdds(l: List[Int]): List[Int] =
    filter(l)(_ % 2 != 0)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def filterImplWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a: A) => if (f(a)) List(a) else Nil)

  def add(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, yss) => yss
    case (xss, Nil) => xss
    case (Cons(x, xss), Cons(y, yss)) => Cons(x + y, add(xss, yss))
  }

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = (xs, ys) match {
    case (Nil, yss) => yss
    case (xss, Nil) => xss
    case (Cons(x, xss), Cons(y, yss)) => Cons(f(x, y), zipWith(xss, yss)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def isPrefix(l: List[A], p: List[A]): Boolean = (l, p) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) isPrefix(xs, ys) else false

    }

    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (xs, ys) if (isPrefix(xs, ys)) => true
      case (Cons(_, xs), ys) => hasSubsequence(xs, ys)
    }
  }
}
