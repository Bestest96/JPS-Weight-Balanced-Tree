object Main {
  def main(args: Array[String]): Unit = {
    Test.testDeletingNodes()

    val tree = TreeGenerator.generateKeys(Stream.from(1).take(20))

    tree.prettyPrintBfs()

    println(tree.isTreeBalanced)

    val set = WBTreeSet[Int](Tree[Int, Nothing]())
  }
}
