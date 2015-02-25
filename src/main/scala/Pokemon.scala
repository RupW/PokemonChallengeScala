import scala.collection.immutable

object Pokemon extends App {

  def readResource(name: String): String = {
    val resource = getClass.getResourceAsStream(name)
    try {
      io.Source.fromInputStream(resource).mkString
    } finally {
      resource.close()
    }
  }

  lazy val allPokemon: immutable.Set[String] = {
    import spray.json._

    val pokemonArray = readResource("pokemon.json").parseJson
    pokemonArray match {
      case JsArray(elements) => elements.map {
        case JsString(value) => value
        case other => throw new IllegalArgumentException(
          s"Expected array of strings; got $other")
      }.toSet
      case _ =>
        throw new IllegalArgumentException("Bad input format")
    }
  }

  val letters = ('a'.asInstanceOf[Int] until 'z'.asInstanceOf[Int]).inclusive.map { _.asInstanceOf[Char] }
  val lettersSet = letters.toSet

  // Look for Pokemon whose letters all appear in a shorter name and remove them
  def pruneLongLetterSubsets(inputSet: immutable.Set[String]): immutable.Set[String] = {
    val inputByLetterSets = letters.map { letter =>
      letter -> inputSet.filter(_.contains(letter)).toSet
    }.toMap
    def distinctLetters(input: String): immutable.Set[Char] = input.toSet & lettersSet
    def sameLetters(input: String): immutable.Set[String] =
      distinctLetters(input).toSeq match {
        case first +: tail => tail.foldLeft(inputByLetterSets(first))((setSoFar, letter) => setSoFar & inputByLetterSets(letter))
        case _ => throw new IllegalArgumentException("no letters in input")
      }
    def shortestInSet(input: immutable.Set[String]) = input.toList.sortWith { (a, b) =>
      (a.length < b.length) || ((a.length == b.length) && (a < b))
    }.head

    inputSet.map{ p => shortestInSet(sameLetters(p)) }.toSet
  }

  val pokemon = pruneLongLetterSubsets(allPokemon)
  println(s"Reduced ${allPokemon.size} to ${pokemon.size} pokemon")

  val pokemonByLetter = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toList.sortBy(_.length)
  }.toMap

  def totalLetters(input: immutable.Set[String]): Int =
    input.foldLeft(0)((lengthSoFar, next) => lengthSoFar + next.length)

  /**
   * Important optimisation: re-order the alphabet that we search through so
   * that the letters with fewest choices are searched first.
   */
  val sortedLetters = letters.toList.sortBy(pokemonByLetter(_).size)

  case class Solution(pokemon: immutable.Set[String], alternatives: immutable.Set[immutable.Set[String]] = Set.empty) {
    val size: Int = pokemon.size
    lazy val length: Int = totalLetters(pokemon)

    def withAlternative(other: Solution) = {
      val newAlternatives = alternatives ++ other.alternatives + other.pokemon - pokemon
      if (!newAlternatives.isEmpty) this.copy(alternatives = newAlternatives)
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
  }

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
  def leastSolution(a: Solution, b: Solution): Solution =
    if (a.size < b.size) a
    else if (b.size < a.size) b
    else {
      // a.size == b.size
      if (a.length < b.length) a
      else if (b.length < a.length) b
      else {
        // a.length == b.length
        b.withAlternative(a)
      }
    }

  def leastSolution(a: Option[Solution], b: Option[Solution]): Option[Solution] = a match {
    case Some(firstSolution) => b match {
      case Some(solution) => Some(leastSolution(firstSolution, solution))
      case None => a
    }
    case None => b
  }

  def findShallowestSolution(maxDepth: Int): Option[Solution] = {
    def leastSolutionRecurse(soFar: immutable.Set[String], soFarCount: Int, soFarLength: Int, letters: immutable.Seq[Char], bestSoFar: Option[Solution]): Option[Solution] = {
      letters match {
        case letter +: moreLetters =>
          if (soFar.exists(_.contains(letter)))
            leastSolutionRecurse(soFar, soFarCount, soFarLength, moreLetters, bestSoFar)
          else if (soFarCount >= maxDepth) {
            // We can't add any more Pokemon to make a better solution
            bestSoFar
          } else {
            val newPokemon = pokemonByLetter(letter).filterNot(soFar.contains(_))

            def tryPokemon(newP: immutable.Seq[String], bestSoFar: Option[Solution]): Option[Solution] = newP match {
              case aPokemon +: morePokemon =>
                val newSolution =
                  if (soFarCount >= maxDepth) {
                    bestSoFar
                  } else if (bestSoFar.isDefined && (soFarCount == bestSoFar.get.size) && ((soFarLength + aPokemon.length) > bestSoFar.get.length))
                    bestSoFar
                  else
                    leastSolutionRecurse(soFar + aPokemon, soFarCount + 1, soFarLength + aPokemon.length, moreLetters, bestSoFar)
                tryPokemon(morePokemon, leastSolution(newSolution, bestSoFar))
              case Nil =>
                bestSoFar
            }
            tryPokemon(newPokemon, bestSoFar)
          }
        case Nil =>
          val newSolution = Some(Solution(soFar))
          val newBest = leastSolution(newSolution, bestSoFar)
          if (newBest != bestSoFar) println(newBest)
          newBest
      }
    }

    leastSolutionRecurse(immutable.Set.empty[String], 0, 0, sortedLetters, None)
  }

  val start = System.currentTimeMillis()
  // TODO parallelise across first letter

  // Breath-first search
  // Create a lazily-evaluated stream of solutions with an increasing maximum depth
  // then take the first non-empty result as our solution
  val solutionStream = (1 until letters.size).inclusive.toStream.map { findShallowestSolution(_) }
  val best: Solution = solutionStream.collectFirst{ case Some(solution) => solution }.get

  // Done!
  val end = System.currentTimeMillis()

  val runtime = end - start
  println(s"Took $runtime ms")
  println(best)
}
