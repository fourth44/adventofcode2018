package aoc2018.day8

object Day8 extends App {

  val lines = () => scala.io.Source.fromFile("src/main/resources/day8.txt").getLines()

  val nums = lines().mkString.split(' ').toSeq.map(_.toInt)

  case class Tree(metadata: Seq[Int], children: Seq[Tree])

  def numsToTrees(nums: Seq[Int], numChildren: Int): (Seq[Tree], Seq[Int]) = {
    (0 until numChildren).foldLeft((Seq[Tree](), nums)) {
      case ((acc, numchild +: nummeta +: rest), _) =>
        val (children, tail2) = numsToTrees(rest, numchild)
        val (meta, tail3) = tail2.splitAt(nummeta)
        val tree = Tree(meta, children)
        (acc :+ tree, tail3)
    }
  }

  val (Seq(tree), Seq()) = numsToTrees(nums, 1)

  def sumMeta(tree: Tree): Int = tree.metadata.sum + tree.children.map(sumMeta).sum

  def sumMeta2(tree: Tree): Int = if (tree.children.isEmpty) {
    tree.metadata.sum
  } else {
    tree.metadata.map(_ - 1).flatMap(i => tree.children.lift(i)).map(sumMeta2).sum
  }

  // part 1
  println(sumMeta(tree))

  // part 2
  println(sumMeta2(tree))

}
