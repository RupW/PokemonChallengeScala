import scala.collection.immutable

object Pokemon extends App with StringSetHelpers {

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

  val pokemon = pruneLongLetterSubsets(allPokemon)
  println(s"Reduced ${allPokemon.size} to ${pokemon.size} pokemon")

  val pokemonByLetter = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toList.sortBy(_.length)
  }.toMap

  /**
   * Important optimisation: re-order the alphabet that we search through so
   * that the letters with fewest choices are searched first.
   */
  val sortedLetters = letters.toList.sortBy(pokemonByLetter(_).size)

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
                tryPokemon(morePokemon, Solution.min(newSolution, bestSoFar))
              case Nil =>
                bestSoFar
            }
            tryPokemon(newPokemon, bestSoFar)
          }
        case Nil =>
          val newSolution = Solution(soFar)
          bestSoFar match {
            case Some(existingSolution) =>
              val newBest = Solution.min(newSolution, existingSolution)
              if (newBest != existingSolution) {
                println(newBest)
                Some(newBest)
              } else {
                bestSoFar
              }
            case None =>
              println(newSolution)
              Some(newSolution)
          }
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
