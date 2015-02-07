package chapter5

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty ⇒ None
    case Cons(h, _) ⇒ Some(h())
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty ⇒ None
    case Cons(_, t) ⇒ Some(t())
  }

  def toList: List[A] = this match {
    case Empty ⇒ Nil
    case Cons(h, t) ⇒ h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n == 0) Empty else this match {
      case e @ Empty ⇒ e
      case Cons(h, t) ⇒ Cons(h, () ⇒ t().take(n - 1))
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 ⇒ Some(h(), (t(), n - 1))
      case _ ⇒ None
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Empty ⇒ Empty
      case c @ Cons(h, t) ⇒ if (n == 0) c else t().drop(n - 1)
    }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) ⇒ Cons(h, () ⇒ t().takeWhile(p))
    case _ ⇒ Empty
  }

  def takeWhile3(p: A ⇒ Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) ⇒ Some(h(), t())
      case _ ⇒ None
    }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B =
    this match {
      case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
      case _ ⇒ z
    }


  def exists(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, b) ⇒ p(a) || b)

  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(false)((a, b) ⇒ p(a) && b)

  def takeWhile2(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else Empty)

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a, _) ⇒ Some(a))

  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ cons(f(a), b))

  def map2[B](f: A ⇒ B): Stream[B] =
    unfold(this) {
      case Empty ⇒ None
      case Cons(h, t) ⇒ Some(f(h()), t())
    }

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else b )

  def append[B >: A](other: ⇒ Stream[B]): Stream[B] =
    foldRight(other)((a, b) ⇒ cons(a, b))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) { case (s1, s2) ⇒
      if (s1.headOption.isEmpty && s2.headOption.isEmpty) None
      else Some((s1.headOption, s2.headOption), (s1.tailOption.getOrElse(Empty), s2.tailOption.getOrElse(Empty)))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).foldRight(true) { (e, s) ⇒
      s && (e._2.isEmpty || e._1 == e._2)
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty ⇒ None
      case c @ Cons(h, t) ⇒ Some(c, t())
    }.append(Stream(empty))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Cons(() ⇒ a, () ⇒ constant(a))

  def from(n: Int): Stream[Int] = Cons(() ⇒ n, () ⇒ from(n + 1))

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None ⇒ Empty
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }

 def unfold2[A, S](z: S)(f: PartialFunction[S, (A, S)]): Stream[A] =
   if (f.isDefinedAt(z)) {
     val (a, s) = f(z)
     cons(a, unfold2(s)(f))
   } else Empty

  def from2(n: Int): Stream[Int] = unfold(n)(i ⇒ Some(i, i + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(i ⇒ Some(i, i))

}