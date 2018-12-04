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


}
