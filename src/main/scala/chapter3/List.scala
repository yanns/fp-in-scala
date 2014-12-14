package chapter3

import scala.annotation.tailrec

sealed trait List[+A] {
  def toString2: String
}
case object Nil extends List[Nothing] {
  override def toString: String = "[]"
  override def toString2: String = "]"
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = "[" + head.toString + tail.toString2
  override def toString2: String = " :: " + head.toString + tail.toString2
}

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new Exception("Nil does not have any tail")
      case Cons(_, xs) => xs
    }

  def setHead[A](a: A, l: List[A]): List[A] =
    Cons(a, tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else
      l match {
        case Nil => throw new Exception("Nil does not have any tail")
        case Cons(_, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
  l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => {
        val newTail = init(t)
        Cons(h, newTail)
      }
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, e) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(acc: B, l: List[A]): B =
      l match {
        case Nil => acc
        case Cons(x, xs) => go(f(acc, x), xs)
      }

    go(z, as)
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((e, acc) => f(acc, e))
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    val inv = foldLeft(as, Nil:List[A])((acc, e) => Cons(e, acc))
    foldLeft(inv, z)((acc, e) => f(e, acc))
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((acc, e) => Cons(e, acc))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(reverse(a1), a2)((e, acc) => Cons(e, acc))
  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc, e) => Cons(e, acc))

  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(reverse(as), Nil:List[A])((acc, e) => append2(e, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil:List[B]
    case Cons(x,xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def go(acc: List[A], l: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(x, xs) =>
          if (f(x)) go(acc, xs)
          else go(Cons(x, acc), xs)
      }

    reverse(go(Nil, as))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    reverse(foldLeft(as, Nil:List[A])((acc, e) => if (f(e)) acc else Cons(e, acc)))

  def filter3[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil:List[A])((e, acc) => if (f(e)) acc else Cons(e, acc))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    @tailrec
    def go(acc: List[B], as: List[A]): List[B] = as match {
      case Nil => acc
      case Cons(x, xs) => go(append(acc, f(x)), xs)
    }

    go(Nil, as)
  }

  def filter4[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Nil else List(a))

  def addLists(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    case _ => Nil
  }

  def zipWith[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] =
    (a1, a2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], toFind: A, rest: List[A], first: Boolean): Boolean = sup match {
      case Nil => false
      case Cons(h, t) if h == toFind => rest match {
        case Nil => true
        case Cons(rh, rt) => go(t, rh, rt, false)
      }
      case Cons(h, t) if first => go(t, toFind, rest, true)
      case _ => false
    }

    sub match {
      case Nil => true
      case Cons(h, t) => go(sup, h, t, true)
    }
  }

}
