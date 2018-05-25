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

  def testDeletingNodes(numberOfTests: Int = 100): Unit = {
    val testResults =
      for(_ <- 1 to numberOfTests) yield {
        val pairs = Stream.from(1).take(1000).map(new Random(_).nextInt(1000)).map((_, Option("XD")))
        var tree = TreeGenerator.generate(pairs)
        val tests =
          for ((key, _) <- pairs) yield {
            tree = tree.delete(tree.find(key).get)
            tree.isTreeBalanced
          }
        if (tests.reduce(_ && _)) 1 else 0
      }

    println("------------delete_nodes------------")
    println(testResults.sum.toString + " out of " + numberOfTests.toString + " tests passed.")
    println("------------------------------------")
  }
}
