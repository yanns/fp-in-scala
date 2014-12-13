import scala.annotation.tailrec

def fibonacci(n: Int): Int = {
  @tailrec
  def go(n: Int, a: Int, b: Int): Int =
    if (n == 0) a
    else go(n-1, b, a+b)
  go(n, 0, 1)
}

fibonacci(0)
fibonacci(1)
fibonacci(2)
fibonacci(3)
fibonacci(4)
fibonacci(5)

@tailrec
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
  if (as.length < 2) true
  else if (!ordered(as(0), as(1))) false
  else isSorted(as.tail, ordered)


isSorted(Array(3, 5 , 6), (a: Int, b: Int) => a > b)
isSorted(Array(3, 5 , 6), (a: Int, b: Int) => a < b)
isSorted(Array(3, 5 , 6, 4), (a: Int, b: Int) => a < b)
isSorted(Array(3, 5 , 6, 6), (a: Int, b: Int) => a < b)
isSorted(Array(3, 5 , 6, 6), (a: Int, b: Int) => a <= b)

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val sum = (a: Int, b: Int) => a + b
sum(3, 4)

val c = curry(sum)
val plus3 = c(3)
plus3(4)

val u = uncurry(c)
u(4, 5)

val f = (x: Double) => math.Pi / 2 - x
val cos = compose(math.sin, f)
cos(1)
math.cos(1)


