package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default : => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map(bb => f(aa,bb)))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val initial: Option[List[A]] = Some(Nil)
    a.foldLeft(initial)((a, b) => map2(a, b)((aa, bb) => bb :: aa))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)((aa) => aa)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val initial: Option[List[B]] = Some(Nil)
    a.foldLeft(initial)((a, b) => map2(a, f(b))((aa, bb) => bb :: aa))
  }
}

case class Some[+A](a: A) extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = Some(f(a))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(a)

  override def getOrElse[B >: A](default: => B): B = a

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(f: (A) => Boolean): Option[A] = if (f(a)) this else None
}

case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
}