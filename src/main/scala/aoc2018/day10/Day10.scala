package aoc2018.day10

object Day10 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day10.txt").getLines()

  val regex = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"""r

  case class Vector(x: Int, y: Int) {
    def plus(other: Vector) = Vector(x + other.x, y + other.y)
    def times(factor: Int) = Vector(x * factor, y * factor)
  }
  case class Star(position: Vector, direction: Vector)

  val instructions = lines().map {
    case regex(x, y, dx, dy) => Star(Vector(x.toInt, y.toInt), Vector(dx.toInt, dy.toInt))
  }.toSeq

  val groupedByVector = instructions.groupBy(_.direction).mapValues(_.map(_.position))

  val groups10 = groupedByVector.filter { case (k, v) =>
      val ys = v.map(_.y)
      ys.max - ys.min == 9 // 'heigth' of example letters is 10... I could figure that out by taking the max height of all teh groups
  }

  val groupedBySize = groupedByVector.groupBy { kv => val ys = kv._2.map(_.y); ys.max - ys.min}

  val group10ys = groups10.map { case (k,v) => k.y -> v.map(_.y).min } // tuples of (y-speed, y-pos)

  val timeDiffs = group10ys.toSeq.combinations(2).map { case Seq((dy1, y1), (dy2, y2)) =>
      val distance = Math.abs(y2 - y1)
      val combinedSpeed = Math.abs(dy1 - dy2)
      distance.toDouble / combinedSpeed.toDouble
  }.toList

  val probableTime = timeDiffs.toSeq.filter(_ >= 0).groupBy(identity).mapValues(_.size).maxBy(_._2)._1.toInt

  println(probableTime)

  val endPositions = instructions.map { case Star(pos, dir) => pos.plus(dir.times(probableTime)) }.toSet

  val (minX, maxX) = { val xs = endPositions.map(_.x); (xs.min, xs.max) }
  val (minY, maxY) = { val ys = endPositions.map(_.y); (ys.min, ys.max) }

  val linesOut = (minY to maxY).map { y =>
    (minX to maxX).map { case x => if (endPositions.contains(Vector(x, y))) {
      " *"
    } else {
      "  "
    }
    }.mkString
  }

  println(linesOut.mkString("\n"))

}
