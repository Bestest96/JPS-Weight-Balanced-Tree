import scala.util.Random

object Test {
  def testAddingNodes(numberOfTests: Int = 100): Unit = {
    val testResults =
    for(_ <- 1 to numberOfTests) yield {
      var tree = Tree[Int, String]()
      val ints = Stream.from(1).take(100).map(new Random(_).nextInt(1000))
      for (i <- ints) {
        val newTree = tree.add(i, Option("XD"))
        tree = newTree
      }
      if (tree.isTreeBalanced) 1 else 0
    }

    println("------------adding_nodes------------")
    println(testResults.sum.toString + " out of " + numberOfTests.toString + " tests passed.")
    println("------------------------------------")
  }
}
