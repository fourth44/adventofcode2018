package aoc2018.day7

import aoc2018._
import aoc2018.tag.@@

object Day7 extends App {

  sealed trait Earlier
  sealed trait Later

  val lines = () => scala.io.Source.fromFile("src/main/resources/day7.txt").getLines()

  val parser = """Step (.) must be finished before step (.) can begin."""r

  val instructions = lines().map { case parser(before, after) => (tag[Later](after), tag[Earlier](before)) }.toList

  val prerequisites: Map[String @@ Later, Set[String @@ Earlier]] = instructions.toSeq.groupByFirst.mapValues(_.toSet)

  def sort(prereq: Map[String @@ Later, Set[String @@ Earlier]]): Seq[Seq[(String @@ Later, Set[String @@ Earlier])]] =
    prereq.toSeq.groupBy(_._2.size).toSeq.sortBy(_._1).map(_._2.sortBy(_._1 : String))

  val start: Map[String @@ Later, Set[String @@ Earlier]] = instructions.toSeq.unzip match {
    case (afters, befores) => (befores.map(x => tag[Later](x: String)).toSet -- afters.toSet).map(_ -> Set[String @@ Earlier]()).toMap
  }

  val traversal = Stream.iterate((Seq[String](), start ++ prerequisites)) { case (acc, prereq) =>
    sort(prereq) match {
      case ((firstZeroReq, empty) +: otherZeroReq) +: others if empty.isEmpty =>
        (acc :+ firstZeroReq, (otherZeroReq +: others).flatten.map { case (x, p) => (x, p - tag[Earlier](firstZeroReq: String)) }.toMap)
      case _ =>
        (acc, Map())
    }
  }

  val sequence = traversal.dropWhile(_._2.nonEmpty).map(_._1)

  // part 1
  println(sequence.headOption.map(_.mkString))

  val numWorkers = 5
  val numExtraSeconds = 60

  sealed trait Seconds

  val timed = Stream.iterate((Seq[String](), Map[String, Int @@ Seconds](), start ++ prerequisites)) {
    case (acc, stepsUnderConstruction, prereq) =>
      val (done, stillBusy) = stepsUnderConstruction.partition(_._2 == 0)
      val workersAvailable = numWorkers - stillBusy.size
      if (workersAvailable == 0) {
        assert(done.isEmpty)
        (acc, stillBusy.mapValues(x => tag[Seconds](x - 1)), prereq)
      } else {
        val foo = sort(prereq.mapValues(_ -- done.keySet.map(tag[Earlier](_: String))))
        sort(prereq.mapValues(_ -- done.keySet.map(tag[Earlier](_: String)))) match {
          case available +: notYetAvailable if available.forall(_._2.isEmpty) =>
            val (starting, postponed) = available.splitAt(workersAvailable)

            val nextBusy = (stillBusy ++ starting.map(_._1).map(s => s -> tag[Seconds](numExtraSeconds + s(0) - 'A' + 1)).toMap).mapValues(x => tag[Seconds](x - 1))
            val nextQueue = (postponed +: notYetAvailable).flatten.toMap

            (acc ++ done.map(_._1).toSeq.sorted, nextBusy, nextQueue)
          case notYetAvailable => // now stuff not being availabe, so simply continue!

            val nextBusy = stillBusy.mapValues(x => tag[Seconds](x - 1))

            (acc ++ done.map(_._1).toSeq.sorted, nextBusy, notYetAvailable.flatten.toMap)
        }
      }
  }

  val (incomplete, complete #:: _) = timed.drop(1).zipWithIndex.span(x => x._1._3.nonEmpty || x._1._2.nonEmpty)

  (incomplete :+ complete).map{ case ((a,b,c),d) => (d, a.mkString, b, c)}.foreach(println)


}
