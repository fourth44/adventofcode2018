package aoc2018.day2

object Day2 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day2.txt").getLines()

  val (twocount, threecount) = lines()
    .map(
      _
        .groupBy(identity).mapValues(_.size)        // count characters: Map[Char, Int]
        .toSeq.groupBy(_._2).mapValues(_.map(_._1)) // inverse multimap: Map[Int,Seq[Char]]
    )
    .foldLeft((0, 0)) { case ((twos, threes), linecounts) =>
      val Seq(addTwo, addThree) = Seq(2,3).map(linecounts.get(_).fold(0)(_ => 1))
      (twos + addTwo, threes + addThree)
    }
  val res1 = twocount * threecount
  println(res1)

  val res2 = lines().toStream.combinations(2)
    .map { case Seq(a, b) => (a zip b).partition(x => x._1 != x._2) }   // which characters are different, wich are the same?
    .collectFirst { case (a,b) if a.size == 1 => b.map(_._1).mkString }

  println(res2)

}
