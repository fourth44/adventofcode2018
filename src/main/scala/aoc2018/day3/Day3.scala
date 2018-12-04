package aoc2018.day3

object Day3 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/aoc2018.day3.txt").getLines()

  val Regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  case class Box(id: Int, left: Int, top: Int, widht: Int, height: Int)

  val boxes = lines().toStream map {
    case Regex(id, l, t, w, h) => Box(id.toInt, l.toInt, t.toInt, w.toInt, h.toInt)
  }

  case class Coord(x: Int, y: Int, belongsTo: Int)

  val coords = for {
    box <- boxes
    x <- box.left until box.left + box.widht
    y <- box.top until box.top + box.height
  } yield {
    Coord(x, y, box.id)
  }

  val claimsByCoordinate = coords.groupBy(c => (c.x, c.y)).mapValues(_.map(_.belongsTo))

  val res1 = claimsByCoordinate.count(_._2.size >= 2)
  println(res1)

  val claimsWithDouble = claimsByCoordinate.collect { case (_, ids) if ids.size >= 2 => ids }.flatten.toSet
  val res2 = boxes.map(_.id).toSet -- claimsWithDouble
  println(res2)


}
