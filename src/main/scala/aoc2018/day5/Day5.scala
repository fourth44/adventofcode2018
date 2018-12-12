package aoc2018.day5

object Day5 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day5.txt").getLines()

  val input = lines().mkString

  def react(polymer: String) = polymer.toSeq.foldLeft(Seq[Char]('0')) { // add dummy
    case (res@(last +: earlier), b) =>
      if (Math.abs(last - b) == 32) earlier else b +: res
  }.drop(1).reverse // remove dummy

  println(react(input).size)

  val res2 = ('A' to 'Z').map { char =>
    react(input.filterNot(c => c == char || c == char + 32)).size
  }.min

  println(res2)

}
