package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val initial: State[Machine, (Int, Int)] = State.unit(0 → 0)
    inputs.foldLeft(initial) { (acc, e) ⇒
      acc.flatMap { case (returnCandies, returnCoins) ⇒
        State { m ⇒
          e match {
            // machine is locked, nothing happened
            case Turn if m.locked ⇒ (returnCandies, returnCoins) → m

            // machine is unlocked - give one candy
            case Turn if !m.locked ⇒ (returnCandies + 1, returnCoins) → m.copy(locked = true, candies = m.candies - 1)

            // no more candies - give coin back
            case Coin if m.candies == 0 ⇒ (returnCandies, returnCoins + 1) → m

            // unlock the machine
            case Coin if m.locked ⇒ (returnCandies, returnCoins) → m.copy(locked = false, coins = m.coins + 1)

            // machine already unlocked - give coin back
            case Coin if !m.locked ⇒ (returnCandies, returnCoins + 1) → m
          }
        }
      }
    }
  }
}
