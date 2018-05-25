import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
//    val pairs = for (i <- 1 to 8) yield (i, Option("XD"))
//    val pairs = for (i <- 1 to 100) yield (i, Option("XD"))
    val pairs = Stream.from(1).take(7).map(new Random(_).nextInt(1000)).map((_, Option("XD")))
    var tree = TreeGenerator.generate(pairs)
    tree.prettyPrintBfs()
    val tests =
    for ((key, value) <- pairs) yield {
      println("Removing" + key.toString)
      tree = tree.delete(tree.find(key).get)
      tree.prettyPrintBfs()
      tree.isTreeBalanced
    }
    val isOK = tests.reduce(_ && _)
    println(isOK)
//
//    tree.prettyPrintBfs()
//
//    println()
//    println()
//
//    newTree.prettyPrintBfs()

//    println(tree.isTreeBalanced)
//    tree.printBfs()
//    tree.printInOrder()
//    tree.prettyPrintBfs()
  }
}
