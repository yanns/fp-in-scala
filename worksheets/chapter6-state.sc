import chapter6._

val rng = SimpleRNG(42)

type Rand[A] = State[RNG, A]

val state = State((rng: RNG) ⇒ rng.nextInt)
state.run(rng)
state.map(_ * 2).run(rng)

val nonNegativeInt: Rand[Int] =
  State { rng ⇒
    val (i, rng2) = rng.nextInt
    if (i >= 0) (i, rng2)
    else nonNegativeInt.run(rng2)
  }

nonNegativeInt.run(rng)

def nonNegativeLessThan(n: Int): Rand[Int] =
  nonNegativeInt.flatMap { i ⇒
    val mod = i % n
    if (i + (n-1) - mod >= 0) State.unit(mod)
    else nonNegativeLessThan(n)
  }

nonNegativeLessThan(33).run(rng)

val int: Rand[Int] = State(_.nextInt)

def ints(count: Int): Rand[List[Int]] =
  State { rng ⇒
    (1 to count).foldLeft((List.empty[Int], rng)) { (tup, _) ⇒
      val (l, r) = tup
      val (i, r2) = r.nextInt
      (i :: l, r2)
    }
  }

ints(3).run(rng)

val ns: Rand[List[Int]] =
  nonNegativeLessThan(100).flatMap(x ⇒
    int.flatMap(y ⇒
      ints(x).map(xs ⇒
        xs.map(_ % y))))

val ns2: Rand[List[Int]] = for {
  x ← nonNegativeLessThan(100)
  y ← int
  xs ← ints(x)
} yield xs.map(_ % y)

ns.run(rng)
ns2.run(rng)
