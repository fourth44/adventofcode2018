package aoc2018.day4

import aoc2018.tag
import aoc2018.tag.@@

object Day4 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/aoc2018.day4.txt").getLines()

  val dateTime = """\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] """r
  val beginShift = """Guard #(\d+) begins shift"""r

  sealed trait Cmd
  case object Sleeps extends Cmd
  case object Wakes extends Cmd
  case class Guard(id: Int) extends Cmd

  sealed trait Minute

  // Let's _not_ model the various parts of the timestamp, we'll only use them for a) sorting and b) extracting the minutes
  type Timestamp = Seq[Int]

  // This import lets me order Seq[Int]s (Timestamps) with priority from left to right (in our case: yyyy, mm, dd, hh, mm)
  import Ordering.Implicits.seqDerivedOrdering

  val commandsSorted: Seq[(Timestamp, Cmd)] = lines().map(_.splitAt(19)).map { case (dateTime(timeParts@_*), command) =>
      val cmd = command match {
        case "falls asleep" => Sleeps
        case "wakes up" => Wakes
        case beginShift(guard) => Guard(guard.toInt)
      }
      timeParts.map(_.toInt) -> cmd
  }.toSeq.sortBy(_._1)

  // All sleep/wake commands paired with their timestamps (as a Seq[Int])
  val commandsByGuard: Map[Guard, Seq[(Timestamp, Cmd)]] =
    commandsSorted.foldLeft((Guard(0), Seq[(Guard, (Timestamp, Cmd))]())) {
      case ((guard, acc), (time, cmd)) =>
        cmd match {
          case g: Guard => (g, acc)                             // set current Guard
          case other => (guard, acc :+ (guard, (time, other)))  // append timestamp + cmd, tupled with current guard to acc
        }
    }._2.groupBy(_._1).mapValues(_.map(_._2))                   // group by guard, strip guard value off the grouped values

  // Validate that each sleep cycle is on the same day, and that the command list is clean ( (Guard (Sleep Wake)*)* )
  require(commandsByGuard.forall(_._2.grouped(2).forall {
    case Seq((t1, Sleeps), (t2, Wakes)) => t1.take(4) == t2.take(4) // year, month, day and hour must be the same
    case _ => false
  }))

  // Histogram per guard of which minute was slept how often
  val minuteCountByGuard: Map[Guard, Map[Int @@ Minute, Int]] = commandsByGuard.mapValues(
    _.grouped(2).map { case Seq((t1, Sleeps), (t2, Wakes)) =>
      (t1(4) until t2(4))                   // minutes between sleep and wake
        .groupBy(min => tag[Minute](min))   // groupBy self, but then 'tagged' with Minute identifier
        .mapValues(_.size)                  // result: histogram of minutes slept during one sleep cycle
    }.foldLeft(Map[Int @@ Minute, Int]()) { case (map1, map2) =>
      // combine all sleep cycle histograms for each guard into one histogram
      map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0)) }
    }
  )

  // part1
  val sleepyGuard: Guard = minuteCountByGuard.maxBy(_._2.map(_._2).sum)._1
  val maxMinute1: Int @@ Minute = minuteCountByGuard(sleepyGuard).maxBy(_._2)._1

  println(sleepyGuard)
  println(maxMinute1)
  println(sleepyGuard.id * maxMinute1)

  // part 2

  val (freqGuard, maxMinute2) = minuteCountByGuard.maxBy(_._2.maxBy(_._2)._2) match {
    case (guard, minuteMap) =>
      val maxMinute = minuteMap.maxBy(_._2)._1
      guard -> maxMinute
  }
  println(freqGuard)
  println(maxMinute2)
  println(freqGuard.id * maxMinute2)

  // I must admit I used three different scala techniques for handing out type names to basic (common) types :
  // * Guard() wraps a guard ID Int. I had to use a wrapper class to make it part of the sealed trait Cmd.
  // * Timestamp is just a type alias for Seq[Int]. Not very typesafe,
  //   but there is only one use case of Seq[Int] here, so its hard to confuse. Only for readability.
  // * Int @@ Minute is a compile time wrapper which is erased at runtime.
  //   This makes it impossible to use a 'regular' Int (or with a different tag) in the wrong place,
  //   while still being able to use it numerically.
}

