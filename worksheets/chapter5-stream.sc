import chapter5._
import chapter5.Stream._

val myStream = Stream(1, 3, 5, 7, 9)
myStream.headOption

myStream.toList
myStream.take(2).toList
myStream.take(0)
myStream.takeWhile(_ < 4).toList

def f(i: Int): Int = { println(s"eval $i"); i }

val m = cons(f(1), cons(f(2), cons(f(3), empty)))
val e = empty
//m.toList
//m.take(2).toList
//m.takeWhile(_ < 2).toList
//m.takeWhile2(_ < 2).toList
m.takeWhile3(_ < 2).toList
//m.exists(_ == 1)
//m.forAll(_ == 1)
//m.headOption2
//e.headOption2
//m.map2(_.toString).toList
//m.filter(_ == 2)

lazy val m2 = m.map(i ⇒ f(i * 10))
m.append(m2).toList
m2.append(empty).toList
empty.append(m2).toList
constant(3).take(3).toList
from(4).drop(10).take(5).toList
val fib: Stream[Long] = {
  def nextFib(a: Long, b: Long): Stream[Long] = cons(a, nextFib(b, a + b))
  nextFib(0, 1)
}
fib.take(20).toList
val s = Stream.unfold(10)(i ⇒ Some(i.toString, i + 1))
s.take(5).toList

val fib2: Stream[Long] = unfold((0l, 1l)) { case (a, b) ⇒ Some(a, (b, a + b)) }
fib2.take(20).toList
from2(10).take2(10).toList
constant2(5).take(10).toList
m.zipAll(myStream).toList
m.startsWith(myStream)
m.startsWith(Stream(1, 2))
m.startsWith(Stream(2, 3))
m.tails.toList(0).toList
m.tails.toList(1).toList
m.tails.toList(2).toList
m.tails.toList(3).toList
myStream.hasSubsequence(Stream(3, 5))
myStream.hasSubsequence(Stream(3, 7))

myStream.scanRight(0)(_ + _).toList