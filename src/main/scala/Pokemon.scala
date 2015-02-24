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
  val worstCaseLength = pokemon.foldLeft(0)((lengthSoFar, next) => lengthSoFar + next.length)

  val sortedLetters = letters.toList.sortBy(pokemonByLetterSets(_).size)

  case class Solution(length: Int, pokemon: Set[String])
  val allPokemon = Solution(worstCaseLength, pokemon)

  def betterSolution(a: Solution, b: Solution): Solution =
    if (a.length < b.length) a else b

  def buildPowerSets(soFar: Set[String], soFarLength: Int, letters: Seq[Char], bestSoFar: Solution): Solution = {
    letters match {
      case letter +: moreLetters =>
        if (soFar.exists(_.contains(letter)))
          buildPowerSets(soFar, soFarLength, moreLetters, bestSoFar)
        else {
          val newPokemon = pokemonByLetter(letter).filterNot(soFar.contains(_))

          def tryPokemon(newP: Seq[String], bestSoFar: Solution): Solution = newP match {
            case aPokemon +: morePokemon =>
              val newSolution =
                if ((soFarLength + aPokemon.length) > bestSoFar.length)
                  bestSoFar
                else
                  buildPowerSets(soFar + aPokemon, soFarLength + aPokemon.length, moreLetters, bestSoFar)
              tryPokemon(morePokemon, betterSolution(newSolution, bestSoFar))
            case Nil =>
              bestSoFar
          }
          tryPokemon(newPokemon, bestSoFar)
        }
      case Nil =>
        if (soFarLength < bestSoFar.length) {
          println(soFarLength.toString + " : " + soFar.mkString(", "))
          Solution(soFarLength, soFar)
        } else {
          bestSoFar
        }
    }
  }

  val start = System.currentTimeMillis()
  // TODO parallelise across first letter
  val solution = buildPowerSets(Set.empty[String], 0, sortedLetters, allPokemon)
  val end = System.currentTimeMillis()
  println(solution)
  val runtime = end - start
  println(s"Took $runtime ms")
}
