import scala.io.StdIn
import scala.util.parsing.combinator.syntactical._

/**
 * Parser for StrongLang.
 */
object StrongLangParser extends StandardTokenParsers {

  val is = "is"

  lexical.reserved += ("bodyweight", "height", "benchpress", "squat", "deadlift", is, "kgs", "lbs", "m", "ft")

  def query = (pHeight).? ~ pBodyWeight ~ pLift.*

  def pBodyWeight = "bodyweight" ~ is ~ weight

  def pHeight = "height" ~ is ~ height

  def pLift = lift ~ is ~ weight

  def lift = "benchpress" |  "squat" | "deadlift"

  def weight = doubleLit ~ weightMeasure

  def height = doubleLit ~ heightMeasure

  def doubleLit = numericLit ~ ("." ~ numericLit).?

  def weightMeasure = "kgs" | "lbs"

  def heightMeasure = "m" | "ft"

  def main(args: Array[String]) = {
    print("StrongLang prompt >>")

    query(new lexical.Scanner(StdIn.readLine)) match {
      case Success(query, _) => println("Yeah!")
      case Failure(msg, _) => println(msg)
      case Error(msg, _) => println(msg)
    }
  }

}
