import chapter4._
import chapter4.Either._

def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

val error = safeDiv(4, 0)
val a = safeDiv(4, 2)
val b = safeDiv(40, 4)

error.map(_ * 2)
a.map(_ * 2)

a.flatMap(aa => safeDiv(4, aa))

error.orElse(a)
error.map2(a)(_ + _)
b.map2(a)(_ + _)

traverse(List(3, 4, 5))(i => safeDiv(42, i))
traverse(List(3, 4, 0))(i => safeDiv(42, i))

sequence(List(Right("hello"), Right("bonjour")))
sequence(List(Right("hello"), Right("bonjour"), Left(45)))
