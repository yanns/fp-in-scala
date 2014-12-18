import chapter4._
import chapter4.Option._

val a = Some("hello")
val b: Option[String] = None
def divideBy(i: Int, j: Int): Option[Int] = {
  if (j == 0) None
  else Some(i / j)
}

a.map(_.length)
b.map(_.length)
a.flatMap(s => divideBy(s.length, 2))
a.flatMap(s => divideBy(s.length, 0))

a.getOrElse("bonjour")
a.getOrElse(throw new Exception("not evaluated"))
b.getOrElse("bonjour")
//b.getOrElse(throw new Exception("evaluated"))
a.orElse(Some("bonjour"))
a.orElse(None)
b.orElse(Some("bonjour"))
b.orElse(None)

a.filter(_.length < 4)
a.filter(_.length >= 4)

val abs = lift(Math.abs)
abs(Some(-45))

sequence(List(Some("abd"), None))
sequence(List(Some("abd"), Some("jdslf")))
sequence2(List(Some("abd"), None))
sequence2(List(Some("abd"), Some("jdslf")))

traverse(List("abc", "sdf"))(s => divideBy(42, s.length))
traverse(List("abc", "sdf", ""))(s => divideBy(42, s.length))