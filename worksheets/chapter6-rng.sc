import chapter6.RNG2.Rand
import chapter6._

val rng = SimpleRNG(42)
val (n1, rng2) = rng.nextInt
val (n2, rng3) = rng2.nextInt
rng3.nextInt
RNG.nonNegativeInt(rng)
RNG.nonNegativeInt(rng2)
RNG.double(rng)
RNG2.double(rng)
RNG.double(rng2)
RNG2.double(rng2)
RNG.intDouble(rng)
RNG2.randIntDouble(rng)
RNG.intDouble(rng2)
RNG2.randIntDouble(rng2)
RNG.doubleInt(rng)
RNG2.randDoubleInt(rng)
RNG.doubleInt(rng2)
RNG2.randDoubleInt(rng2)
RNG.double3(rng)
RNG.double3(rng2)
RNG.int(5)(rng)
RNG.int(5)(rng2)
val l: List[Rand[Int]] = List.fill(4)(RNG2.int)
RNG2.sequence(l)(rng)
RNG2.nonNegativeLessThan(999)(rng)
RNG2.nonNegativeLessThan2(999)(rng)