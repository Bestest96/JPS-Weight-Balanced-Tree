object Main {
  def main(args: Array[String]): Unit = {
//    Test.testDeletingNodes()
//
//    val tree = TreeGenerator.generateFromKeys(Stream.from(1).take(20))
//
//    tree.prettyPrintBfs()
//
//    println(tree.isTreeBalanced)

//    val ourSet = List(1, 2, 3, 4, 5).foldLeft(WBTreeSet[Int]())((B: WBTreeSet[Int], key: Int) => B.add(key))
////    val ourSet = WBTreeSet[Int]().add(3).add(4).add(5)
////    val ourSet2 = WBTreeSet[Int]().add(3).add(7).add(-1)
//    println(ourSet.getAllElements)

    Test.testWBTreeSet()
  }
}
