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

  lazy val pokemon: immutable.Set[String] = {
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

  // TODO let's remove pokemon where any other has the same or greater set of letters in a shorter name

  val letters = ('a'.asInstanceOf[Int] until 'z'.asInstanceOf[Int]).inclusive.map { _.asInstanceOf[Char] }
  val pokemonByLetter = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toList.sortBy(_.length)
  }.toMap
  val pokemonByLetterSets = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toSet
  }.toMap

  def totalLetters(input: Set[String]): Int =
    input.foldLeft(0)((lengthSoFar, next) => lengthSoFar + next.length)

  val sortedLetters = letters.toList.sortBy(pokemonByLetterSets(_).size)

  case class Solution(pokemon: Set[String], alternatives: Set[Set[String]] = Set.empty) {
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

  val allPokemon = Solution(pokemon)

  def shorterSolution(a: Solution, b: Solution): Solution =
    if (a.length < b.length) a else b

  def shortestSolutionRecurse(soFar: Set[String], soFarLength: Int, letters: Seq[Char], bestSoFar: Solution): Solution = {
    letters match {
      case letter +: moreLetters =>
        if (soFar.exists(_.contains(letter)))
          shortestSolutionRecurse(soFar, soFarLength, moreLetters, bestSoFar)
        else {
          val newPokemon = pokemonByLetter(letter).filterNot(soFar.contains(_))

          def tryPokemon(newP: Seq[String], bestSoFar: Solution): Solution = newP match {
            case aPokemon +: morePokemon =>
              val newSolution =
                if ((soFarLength + aPokemon.length) > bestSoFar.length)
                  bestSoFar
                else
                  shortestSolutionRecurse(soFar + aPokemon, soFarLength + aPokemon.length, moreLetters, bestSoFar)
              tryPokemon(morePokemon, shorterSolution(newSolution, bestSoFar))
            case Nil =>
              bestSoFar
          }
          tryPokemon(newPokemon, bestSoFar)
        }
      case Nil =>
        if (soFarLength < bestSoFar.length) {
          val newSolution = Solution(soFar)
          println(newSolution)
          newSolution
        } else {
          bestSoFar
        }
    }
  }

  def shortestSolution(bestSoFar: Solution): Solution =
    shortestSolutionRecurse(Set.empty[String], 0, sortedLetters, bestSoFar)

  def betterSolution(a: Solution, b: Solution): Solution =
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

  def bestSolutionRecurse(soFar: Set[String], soFarCount: Int, soFarLength: Int, letters: Seq[Char], bestSoFar: Solution): Solution = {
    letters match {
      case letter +: moreLetters =>
        if (soFar.exists(_.contains(letter)))
          bestSolutionRecurse(soFar, soFarCount, soFarLength, moreLetters, bestSoFar)
        else if (soFarCount >= bestSoFar.size) {
          // We can't add any more Pokemon to make a better solution
          bestSoFar
        } else {
          val newPokemon = pokemonByLetter(letter).filterNot(soFar.contains(_))

          def tryPokemon(newP: Seq[String], bestSoFar: Solution): Solution = newP match {
            case aPokemon +: morePokemon =>
              val newSolution =
                if (soFarCount >= bestSoFar.size) {
                  bestSoFar
                } else if ((soFarCount == bestSoFar.size) && ((soFarLength + aPokemon.length) > bestSoFar.length))
                  bestSoFar
                else
                  bestSolutionRecurse(soFar + aPokemon, soFarCount + 1, soFarLength + aPokemon.length, moreLetters, bestSoFar)
              tryPokemon(morePokemon, betterSolution(newSolution, bestSoFar))
            case Nil =>
              bestSoFar
          }
          tryPokemon(newPokemon, bestSoFar)
        }
      case Nil =>
        val newSolution = Solution(soFar)
        val newBest = betterSolution(newSolution, bestSoFar)
        if (newBest != bestSoFar) println(newBest)
        newBest
    }
  }

  def bestSolution(bestSoFar: Solution): Solution =
    bestSolutionRecurse(Set.empty[String], 0, 0, sortedLetters, bestSoFar)

  val start = System.currentTimeMillis()
  // TODO parallelise across first letter
  val shortest = shortestSolution(allPokemon)
  println
  val best = bestSolution(shortest)
  val end = System.currentTimeMillis()

  val runtime = end - start
  println(s"Took $runtime ms")
  println(best)
}
