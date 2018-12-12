package aoc2018.day9

import aoc2018._

object Day9 extends App {
  val numPlayers = 458
  val lastMarbleValue = 71307 // remove *100 for part 1

  val lookback = 7 // this lookback doesn't seem to matter much, shiftLeft supposedly cheap enough

  case class Circle[A](left: List[A], current: A, right: List[A]) {
    def shiftRight: Circle[A] = {
      right match {
        case r1 :: rn => Circle(current :: left, r1, rn)
        case Nil =>
          val numKeepLeft = left.size - Math.min(left.size, Math.max(1, left.size - lookback - 1))
          val (keepLeft, move) = left.splitAt(numKeepLeft)
          if (move.isEmpty) {
            this
          } else {
            Circle(keepLeft, current, move.reverse).shiftRight
          }

      }
    }
    def reverse: Circle[A] = Circle(right, current, left)
    def shiftLeft: Circle[A] = reverse.shiftRight.reverse
    def insertBefore(newCurr: A): Circle[A] = Circle(left, newCurr, current +: right)

    // dangerous operation, may fail if removing last element!
    def remove: (A, Circle[A]) = shiftRight.shiftLeft.right match {
      case head +: tail => (current, Circle(left, head, tail))
    }
    override def toString = s"${left.reverse.mkString(" ")} (${current}) ${right.mkString(" ")}"
  }

  val cirles = Iterator.iterate((Circle(List(), 0, List()), 1, Map[Int, Long]())) {
    case (circle, marble, scores) =>
      val currentElf = (marble % numPlayers).toInt
      val (newCircle, elfScore) = marble match {
        case n if n % 23 == 0 =>
          val rotated = (1 to 7).foldLeft(circle)((c, _) => c.shiftLeft)
          val (marbleOut, c2) = rotated.remove
          (c2, marbleOut + marble)
        case _ =>
          (circle.shiftRight.shiftRight.insertBefore(marble), 0)

      }
      (newCircle, marble + 1, scores.updated(currentElf, scores.getOrElse(currentElf, 0l) + elfScore))
  }

  val (time, res) = timed {
    cirles.drop(lastMarbleValue).next()
  }
  println(res._2)
  println(res._3.maxBy(_._2))
  println(time)

}
