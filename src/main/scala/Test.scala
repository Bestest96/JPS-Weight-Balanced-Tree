import scala.util.Random

/**
  * Class containing tests covering core functions of Tree class
  * as well as WBTreeSet functions.
  */
object Test {
  def testAll(): Unit = {
    testAddingNodes()
    testDeletingNodes()
    testWBTreeSet()
  }

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
        val keys = Stream.from(1).take(1000).map(new Random(_).nextInt(1000))
        var tree = TreeGenerator.generateFromKeys(keys)
        val tests =
          for (key <- keys) yield {
            tree = tree.delete(key)
            tree.isTreeBalanced
          }
        if (tests.reduce(_ && _)) 1 else 0
      }

    println("-----------deleting_nodes-----------")
    println(testResults.sum.toString + " out of " + numberOfTests.toString + " tests passed.")
    println("------------------------------------")
  }

  def testWBTreeSet(): Unit = {
    testWBTreeSetElementsAfterDeletion()
    testWBTreeSetElementsAfterDeletions()
    testWBTreeSetElementsAfterDeletionNotExistingValue()
    testWBTreeSetElementsAfterOneAdding()
    testWBTreeSetContains()
    testWBTreeSetSumEmptySets()
    testWBTreeSetSumNotEmptySets()
    testWBTreeSetSumOneEmptySet()
    testWBTreeSetIntersectionEmptySets()
    testWBTreeSetIntersectionNotEmptySetsEmptyIntersection()
    testWBTreeSetIntersectionNotEmptySetsNotEmptyIntersection()
    testWBTreeSetIntersectionOneEmptySet()

    println("--------------WBTreeSet-------------")
    println("All tests passed.")
    println("------------------------------------")
  }

  private def testWBTreeElementsEmptySet(): Unit = {
    val mySet = WBTreeSet[Int]()
    assert(mySet.getAllElements == List())
  }

  private def testWBTreeSetElementsAfterOneAdding(): Unit = {
    val mySet = WBTreeSet[Int]().add(1)
    assert(mySet.getAllElements == List(1))
  }

  private def testWBTreeSetElementsAfterDeletion(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).delete(1)
    assert(mySet.getAllElements == List())
  }

  private def testWBTreeSetElementsAfterDeletions(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).add(3).add(4)
    val mySet2 = mySet.delete(2).delete(3)
    assert(mySet2.getAllElements.toSet == List(1, 4).toSet)
  }

  private def testWBTreeSetElementsAfterDeletionNotExistingValue(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).delete(3).delete(4)
    assert(mySet.getAllElements.toSet == Set(1, 2))
  }

  private def testWBTreeSetContains(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).add(3).delete(2)
    assert(mySet.contains(1) && !mySet.contains(2) && mySet.contains(3))
  }

  private def testWBTreeSetSumEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]()
    assert(mySet1.sum(mySet2).getAllElements == List())
  }

  private def testWBTreeSetSumOneEmptySet(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]().add(1).add(2).add(3)
    assert(mySet1.sum(mySet2).getAllElements.toSet == List(1, 2, 3).toSet)
  }

  private def testWBTreeSetSumNotEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2)
    val mySet2 = WBTreeSet[Int]().add(3)
    assert(mySet1.sum(mySet2).getAllElements.toSet == List(1, 2, 3).toSet)
  }

  private def testWBTreeSetIntersectionEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]()
    assert(mySet1.intersection(mySet2).getAllElements == List())
  }

  private def testWBTreeSetIntersectionOneEmptySet(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]().add(1).add(2)
    assert(mySet1.intersection(mySet2).getAllElements == List())
  }

  private def testWBTreeSetIntersectionNotEmptySetsNotEmptyIntersection(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2).add(3)
    val mySet2 = WBTreeSet[Int]().add(1).add(7).add(3)
    assert(mySet1.intersection(mySet2).getAllElements.toSet == List(1, 3).toSet)
  }

  private def testWBTreeSetIntersectionNotEmptySetsEmptyIntersection(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2).add(3)
    val mySet2 = WBTreeSet[Int]().add(4).add(5).add(6)
    assert(mySet1.intersection(mySet2).getAllElements == List())
  }

}
