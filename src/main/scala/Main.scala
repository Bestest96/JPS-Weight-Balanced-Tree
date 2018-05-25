object Main {
  def main(args: Array[String]): Unit = {
    var tree = Tree[Int, String]()
//    val ints = List(3, 6, 12, 28, 5, 36, 42, 16, 36, 10)
//    val ints = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    for (i <- 1 to 30) {
      val newTree = tree.add(i, Option("XD"))
      tree = newTree
    }

//    tree.printBfs()
//    tree.printInOrder()
    tree.prettyPrintBfs()

    println(tree.isTreeBalanced)
  }
}
