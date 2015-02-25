import scala.collection.immutable

trait StringSetHelpers {
  val letters = ('a'.asInstanceOf[Int] until 'z'.asInstanceOf[Int]).inclusive.map { _.asInstanceOf[Char] }
  val lettersSet = letters.toSet

  def distinctLetters(input: String): immutable.Set[Char] = input.toSet & lettersSet

  def shortestInSet(input: immutable.Set[String]) = input.toList.sortWith { (a, b) =>
    (a.length < b.length) || ((a.length == b.length) && (a < b))
  }.head

  // Look for Pokemon whose letters all appear in a shorter name and remove them
  def pruneLongLetterSubsets(inputSet: immutable.Set[String]): immutable.Set[String] = {
    val inputByLetterSets = letters.map { letter =>
      letter -> inputSet.filter(_.contains(letter)).toSet
    }.toMap

    def sameLetters(input: String): immutable.Set[String] =
      distinctLetters(input).toSeq match {
        case first +: tail => tail.foldLeft(inputByLetterSets(first))((setSoFar, letter) => setSoFar & inputByLetterSets(letter))
        case _ => throw new IllegalArgumentException("no letters in input")
      }

    inputSet.map{ p => shortestInSet(sameLetters(p)) }.toSet
  }
}
