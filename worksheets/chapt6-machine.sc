import chapter6._

Machine.simulateMachine(List(Coin, Turn, Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 10, 4))

Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 2, 4))