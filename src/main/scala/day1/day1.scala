package day1

object day1 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day1.txt").getLines().map(_.toInt)

  val res1 = lines().sum
  println(res1)

  val cycle: Stream[Int] = lines().toStream #::: cycle
  val runningSums = cycle.scanLeft(0)(_ + _)
  val runningSets = runningSums.scanLeft(Set[Int]())(_ + _)
  val zipped = runningSets zip runningSums.drop(1)
  val res2 = zipped collectFirst { case (set, freq) if set contains freq => freq }
  println(res2)
}
