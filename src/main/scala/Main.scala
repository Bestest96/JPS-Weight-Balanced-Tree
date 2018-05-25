import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val pairs = for (i <- 1 to 15) yield (i, Option("XD"))
    val tree = TreeGenerator.generate(pairs)

//    println(tree.isTreeBalanced)
//    tree.printBfs()
//    tree.printInOrder()
    tree.prettyPrintBfs()
  }
}
