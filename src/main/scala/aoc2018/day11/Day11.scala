package aoc2018.day11

object Day11 extends App {

  val serial = 7989

  case class Vector(x: Int, y: Int) {
    def plus(other: Vector) = Vector(x + other.x, y + other.y)
  }

  val allVectors = for { y <- 1 to 300; x <- 1 to 300 } yield { Vector(x, y) }

  val powers = allVectors.map { case v@Vector(x, y) =>
    val rackId = x + 10
    val pwr = ((rackId * y) + serial) * rackId
    val pwr2 = pwr.toString.dropRight(2).takeRight(1).toInt - 5
    (v, pwr2)
  }.toMap

  // comonadic store
  case class Store[S,A](peek: S => A, cursor: S) {
    def extract: A = peek(cursor)
    def extend[B](f: Store[S,A] => B): Store[S, B] =
      Store(s => f(Store(peek, s)), cursor)
    def map[B](f: A => B): Store[S,B] =
      extend(s => f(s.extract))
  }

  val powerStore = Store[Vector, Int](peek = powers.apply, cursor = Vector(1,1))

  // part 1 implementations fixed to 3x3
  val surroundings = for {
    y <- 0 to 2
    x <- 0 to 2
  } yield {
    Vector(x,y)
  }

  val grid3x3PowerStore = powerStore.extend(store => surroundings.map(sur => store.peek(sur.plus(store.cursor))))
  val grid3x3PowerSumStore = grid3x3PowerStore.map(_.sum)

  val powerList = for {
    x <- 1 to 300 - 2
    y <- 1 to 300 - 2
  } yield {
    (x, y, grid3x3PowerSumStore.peek(Vector(x, y)))
  }
  val (maxPowX, maxPowY, maxPow) = powerList.maxBy(_._3)
  println(s"$maxPowX,$maxPowY")

  // part 2 implementations, N x N

  def surroundingsN(size: Int) = for {
    y <- 0 until size
    x <- 0 until size
  } yield {
    Vector(x,y)
  }
  def gridNPowerStore(size: Int) = {
    val surrN = surroundingsN(size)
    powerStore.extend(store => surrN.map(sur => store.peek(sur.plus(store.cursor))))
  }
  def gridNPowerSumStore(size: Int) = gridNPowerStore(size).map(_.sum)

  def powerList2 = for {
    size <- (1 to 300).toVector
    _ = println(s"size $size")
    powerSumGrid = gridNPowerSumStore(size)
    x <- 1 to 300 - size + 1
    y <- 1 to 300 - size + 1
  } yield {
    if (x % 20 == 0 && y % 20 == 0) {
      println(s"$size, $x, $y")
    }
    (x, y, size, powerSumGrid.peek(Vector(x, y)))
  }
  val (maxPowX2, maxPowY2, size, maxPow2) = powerList2.maxBy(_._4)
  println(s"$maxPowX2,$maxPowY2,$size")

  // I could do with a whole lot less of re-calculation.
  // start with the power.
  // each time add another 'shell' to the sum. maybe as an option, undefined when you walk out. flatmap the options in each step.
  // OR do prime factorization. make grids for the prime numbers, for any other grid sum multitudes of them.




}
