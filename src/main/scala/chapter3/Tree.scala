package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(b1, b2) => 1 + size(b1) + size(b2)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(b1, b2) => maximum(b1) max maximum(b2)
    case Leaf(v) => v
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(b1, b2) => (1 + depth(b1)) max (1 + depth(b2))
    case Leaf(_) => 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(b1,b2) => Branch(map(b1)(f), map(b2)(f))
    }

  def fold[A,B](t: Tree[A])(f: A => B, g: (B,B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(b1,b2) => g(fold(b1)(f, g), fold(b2)(f, g))
    }

  def size2[A](t: Tree[A]): Int = fold[A,Int](t)(_ => 1, (a,b) => 1 + a + b)

  def maximum2(t: Tree[Int]): Int = fold[Int,Int](t)(v => v, _ max _)

  def depth2[A](t: Tree[A]): Int = fold[A,Int](t)(_ => 1, (a,b) => (1+a) max (1+b))


  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(v => Leaf(f(v)), (a,b) => Branch(a,b))
}