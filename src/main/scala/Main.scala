object Main {
  def main(args: Array[String]): Unit = {
    var tree = Tree[Int, String]()
    val ints = List(3, 6, 12, 28, 5, 36, 42, 16, 36, 10)
    for (i <- ints) {
      val newTree = tree.add(i, Option("XD"))
      tree = newTree
    }

    tree.printBfs()

    tree.printInOrder()
  }
}
