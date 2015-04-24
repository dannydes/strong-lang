import scala.io.StdIn
import scala.util.parsing.combinator._

/**
 * Parser for StrongLang.
 */
object StrongLangParser extends JavaTokenParsers {

  val is = "is"

  def query = pHeight.? ~ pBodyWeight ~ pLift.*

  def pBodyWeight = "bodyweight" ~ is ~ weight

  def pHeight = "height" ~ is ~ height

  def pLift = lift ~ is ~ weight

  def lift = "benchpress" | "squat" | "deadlift"

  def weight = decimalNumber ~ weightMeasure

  def height = decimalNumber ~ heightMeasure

  def weightMeasure = "kgs" | "lbs"

  def heightMeasure = "m" | "ft"

  def main(args: Array[String]) = {
    print("StrongLang prompt >>")

    parseAll(query, StdIn.readLine) match {
      case Success(query, _) => println("Yeah!")
      case Failure(msg, _) => println(msg)
      case Error(msg, _) => println(msg)
    }
  }

}
