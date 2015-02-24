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

  val letters = ('a'.asInstanceOf[Int] until 'z'.asInstanceOf[Int]).inclusive.map { _.asInstanceOf[Char] }
  val pokemonByLetter = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toList.sortBy(_.length)
  }.toMap
  val pokemonByLetterSets = letters.map { letter =>
    letter -> pokemon.filter(_.contains(letter)).toSet
  }.toMap
  val worstCaseLength = pokemon.foldLeft(0)((lengthSoFar, next) => lengthSoFar + next.length)

  case class Solution(length: Int, pokemon: Set[String])
  val allPokemon = Solution(worstCaseLength, pokemon)

  def betterSolution(a: Solution, b: Solution): Solution =
    if (a.length < b.length) a else b

  def buildPowerSets(soFar: Set[String], soFarConcat: String, letters: Seq[Char], bestSoFar: Solution): Solution = {
    letters match {
      case letter +: moreLetters =>
        if (soFarConcat.contains(letter))
          buildPowerSets(soFar, soFarConcat, moreLetters, bestSoFar)
        else {
          val newPokemon = pokemonByLetter(letter).filterNot(soFar.contains(_))
          val soFarLength = soFarConcat.length

          def tryPokemon(newP: Seq[String], bestSoFar: Solution): Solution = newP match {
            case aPokemon +: morePokemon =>
              val newSolution =
                if ((soFarLength + aPokemon.length) > bestSoFar.length)
                  bestSoFar
                else
                  buildPowerSets(soFar + aPokemon, soFarConcat + aPokemon, moreLetters, bestSoFar)
              tryPokemon(morePokemon, betterSolution(newSolution, bestSoFar))
            case Nil =>
              bestSoFar
          }
          tryPokemon(newPokemon, bestSoFar)
        }
      case Nil =>
        if (soFarConcat.length < bestSoFar.length)
          Solution(soFarConcat.length, soFar)
        else
          bestSoFar
    }
  }

  val solution = buildPowerSets(Set.empty[String], "", letters, allPokemon)
  val start = System.currentTimeMillis()
  println(solution)
  val end = System.currentTimeMillis()
  println(s"Took ${end-start} ms")
}
