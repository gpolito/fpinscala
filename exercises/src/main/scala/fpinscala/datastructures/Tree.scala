package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumWithFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthWithFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((x, y) => (x max y) + 1)

  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((x, y) => Branch(x, y))
}
