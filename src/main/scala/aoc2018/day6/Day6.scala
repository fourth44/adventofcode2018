package aoc2018.day6

import aoc2018.invert

object Day6 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day6.txt").getLines()

  val regex = """(\d+), (\d+)"""r

  case class Point(x: Int, y: Int) {
    def neighbours: Seq[Point] = Seq(Point(x+1, y), Point(x-1, y), Point(x, y+1), Point(x, y-1))
  }

  val points = lines().map { case regex(x, y) => Point(x.toInt, y.toInt) }.toSeq

  val minX = points.minBy(_.x).x
  val maxX = points.maxBy(_.x).x
  val minY = points.minBy(_.y).y
  val maxY = points.maxBy(_.y).y
  // val maxSize = (maxX - minX) * (maxY - minY)

  val distances = (for {
    x <- minX to maxX
    y <- minY to maxY
    gridPoint = Point(x,y)
    centerPoint <- points
  } yield {
    (gridPoint, (centerPoint, Math.abs(x - centerPoint.x) + Math.abs(y - centerPoint.y))) // for each coord, distance to some given center point
  }).groupByFirst

  val closestCenterPointsPerGridPoint = distances.mapValues { allDistances =>
    val minDist = allDistances.minBy(_._2)._2
    allDistances.filter(_._2 == minDist).map(_._1).toSet // collect...
  }
  val borderPoints = closestCenterPointsPerGridPoint.filterKeys(p => p.x == minX || p.x == maxX || p.y == minY || p.y == maxY).values.toSet.flatten
  val uniqueClosestsPointsPerCenterPoint = invert(closestCenterPointsPerGridPoint.filter(_._2.size == 1))
  val nonBorderGroups = uniqueClosestsPointsPerCenterPoint.filterKeys(center => !borderPoints.contains(center))
  val res1 = nonBorderGroups.mapValues(_.size).toSeq.maxBy(_._2)
  println(res1)

  val summedDistancesPerGridPoint = distances.mapValues(_.map(_._2).sum)
  val under10000 = summedDistancesPerGridPoint.filter(_._2 < 10000)
  println(under10000.size)
  
}
