import chapter4._
import chapter4.Option._

def parseInsuranceRateQuote(
  age: String,
  numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try { age.toInt }
  val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
  map2(optAge, optTickets)(insuranceRateQuote)
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 42

parseInsuranceRateQuote("34", "ad")
parseInsuranceRateQuote("34", "56")