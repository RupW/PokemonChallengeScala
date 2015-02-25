import scala.collection.immutable

case class Solution(pokemon: immutable.Set[String], alternatives: immutable.Set[immutable.Set[String]] = Set.empty) {
  import Solution._

  val size: Int = pokemon.size
  lazy val length: Int = totalLetters(pokemon)

  def withAlternative(other: Solution) = {
    val newAlternatives = alternatives ++ other.alternatives + other.pokemon - pokemon
    if (newAlternatives != alternatives) this.copy(alternatives = newAlternatives)
    else this
  }

  override def toString = {
    val thisSet = s"$size, $length : " + pokemon.mkString(", ")
    if (alternatives.isEmpty) thisSet
    else {
      val alternativesString = alternatives.map(_.mkString(", ")).mkString(" or ")
      s"$thisSet ($alternativesString)"
    }
  }

  def compareTo(other: Solution): Int = {
    val sizeDiff = size.compareTo(other.size)
    if (sizeDiff != 0)
      sizeDiff
    else
      length.compareTo(other.length)
  }
}

object Solution {

  /**
   * Compare the two sets and return the one that is smaller in the defined
   * Pokemon set ordering, or both solutions if tied.
   *
   * The least is defined as the one with fewest entries, or if tied the one
   * with fewest total letters. If this is still a tie, we return the second
   * solution but with the first attached as an alternative equivalent
   * solution.
   *
   * @param a the first solution to compare; when combining equivalent
   *          solutions this is assumed to be the new candidate solution
   * @param b the second solution to compare; when combining equivalent
   *          solutions this is assumed to be the older existing best solution
   *          so-far
   * @return the better solution, by the problem's comparison rules, or the
   *         second solution with the first attached as a new alternative
   *         answer if equal.
   */
  def min(a: Solution, b: Solution): Solution = {
    val compare = a.compareTo(b)
    if (compare < 0) a
    else if (compare > 0) b
    else b.withAlternative(a)
  }

  def min(a: Option[Solution], b: Option[Solution]): Option[Solution] = a match {
    case Some(firstSolution) => b match {
      case Some(solution) => Some(min(firstSolution, solution))
      case None => a
    }
    case None => b
  }

  private def totalLetters(input: immutable.Set[String]): Int =
    input.foldLeft(0)((lengthSoFar, next) => lengthSoFar + next.length)
}
