package object aoc2018 {

  // Borrowed from Shapeless:
  // Allows tagging values with a marker type. Useful for e.g. distinguishing multiple 'Int's in a type, like Map[Int @@ ElfId, Int @@ HoCount]
  // Similar typesafety compared to wrapper classes, but with less unpacking required.
  object tag {
    def apply[U] = new Tagger[U]

    trait Tagged[U]
    type @@[+T, U] = T with Tagged[U]

    class Tagger[U] {
      def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
    }
  }

  implicit class Grouper[A, B](seq: Iterable[(A, B)]) {
    def groupByFirst: Map[A, Seq[B]] = seq.groupBy(_._1).mapValues(_.map(_._2).toSeq)
  }

  def invert[A,B](map: Iterable[(A, Set[B])]): Map[B, Seq[A]] = {
    (for {
      (point, claims) <- map
      claim <- claims
    } yield {
      (claim, point)
    }).groupByFirst
  }

  def timed[A](thunk: => A): (Long, A) = {
    val t1 = System.currentTimeMillis()
    val res = thunk
    val t2 = System.currentTimeMillis()
    (t2 - t1, res)
  }

}
