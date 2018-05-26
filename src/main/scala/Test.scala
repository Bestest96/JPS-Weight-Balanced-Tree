import scala.util.Random

/**
  * Class containing tests covering core functions of Tree class
  * as well as WBTreeSet functions.
  */
object Test {
  /**
    * Launches all tests.
    */
  def testAll(): Unit = {
    testAddingNodes()
    testDeletingNodes()
    testWBTreeSet()
  }

  /**
    * Tests adding nodes to the tree.
    * @param numberOfTests Number of tests to perform.
    */
  def testAddingNodes(numberOfTests: Int = 100): Unit = {
    val testResults =
    for(_ <- 1 to numberOfTests) yield {
      val tree = TreeGenerator.generateKeys(Stream.from(1).take(1000).map(new Random(_).nextInt(10000)))
      if (tree.isTreeBalanced) 1 else 0
    }

    println("------------adding_nodes------------")
    println(testResults.sum.toString + " out of " + numberOfTests.toString + " tests passed.")
    println("------------------------------------")
  }

  /**
    * Tests deleting nodes from the tree.
    * @param numberOfTests Number of tests to perform.
    */
  def testDeletingNodes(numberOfTests: Int = 100): Unit = {
    val testResults =
      for(_ <- 1 to numberOfTests) yield {
        val keys = Stream.from(1).take(1000).map(new Random(_).nextInt(1000))
        val tree = TreeGenerator.generateKeys(keys)
        /**
          * Deletes all keys from a tree and saves result into stream.
          * @param tree Actual tree.
          * @param keys Remaining keys to be deleted.
          * @return A stream of trees.
          */
        def loop(tree: Tree[Int, Nothing], keys: Stream[Int]): Stream[Tree[Int, Nothing]] = tree #:: (if(keys.nonEmpty) loop(tree.delete(keys.head), keys.tail) else Stream.Empty)
        val tests = loop(tree, keys).toList.map(_.isTreeBalanced)
        if (tests.reduce(_ && _)) 1 else 0
      }

    println("-----------deleting_nodes-----------")
    println(testResults.sum.toString + " out of " + numberOfTests.toString + " tests passed.")
    println("------------------------------------")
  }

  /**
    * Tests operations on WBTreeSet.
    */
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

  /**
    * Tests if created set is empty.
    */
  private def testWBTreeElementsEmptySet(): Unit = {
    val mySet = WBTreeSet[Int]()
    assert(mySet.getAllElements == List())
  }

  /**
    * Tests if set with one added element has one element.
    */
  private def testWBTreeSetElementsAfterOneAdding(): Unit = {
    val mySet = WBTreeSet[Int]().add(1)
    assert(mySet.getAllElements == List(1))
  }
  /**
    * Tests if set with one added and deleted element is empty.
    */
  private def testWBTreeSetElementsAfterDeletion(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).delete(1)
    assert(mySet.getAllElements == List())
  }
  /**
    * Tests if created set with additions and deletions has given elements.
    */
  private def testWBTreeSetElementsAfterDeletions(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).add(3).add(4)
    val mySet2 = mySet.delete(2).delete(3)
    assert(mySet2.getAllElements.toSet == List(1, 4).toSet)
  }
  /**
    * Tests if created set with additions and deletions on nonexisten elements has given elements.
    */
  private def testWBTreeSetElementsAfterDeletionNotExistingValue(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).delete(3).delete(4)
    assert(mySet.getAllElements.toSet == Set(1, 2))
  }
  /**
    * Tests if a set with added elements contains given elements.
    */
  private def testWBTreeSetContains(): Unit = {
    val mySet = WBTreeSet[Int]().add(1).add(2).add(3).delete(2)
    assert(mySet.contains(1) && !mySet.contains(2) && mySet.contains(3))
  }
  /**
    * Tests if sum of two empty sets is empty.
    */
  private def testWBTreeSetSumEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]()
    assert(mySet1.sum(mySet2).getAllElements == List())
  }
  /**
    * Tests if sum of two sets gives proper results.
    */
  private def testWBTreeSetSumOneEmptySet(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]().add(1).add(2).add(3)
    assert(mySet1.sum(mySet2).getAllElements.toSet == List(1, 2, 3).toSet)
  }
  /**
    * Tests if sum of two sets gives proper results.
    */
  private def testWBTreeSetSumNotEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2)
    val mySet2 = WBTreeSet[Int]().add(3)
    assert(mySet1.sum(mySet2).getAllElements.toSet == List(1, 2, 3).toSet)
  }
  /**
    * Tests if intersection of two empty sets is empty.
    */
  private def testWBTreeSetIntersectionEmptySets(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]()
    assert(mySet1.intersect(mySet2).getAllElements == List())
  }
  /**
    * Tests if intersection of two sets gives proper results.
    */
  private def testWBTreeSetIntersectionOneEmptySet(): Unit = {
    val mySet1 = WBTreeSet[Int]()
    val mySet2 = WBTreeSet[Int]().add(1).add(2)
    assert(mySet1.intersect(mySet2).getAllElements == List())
  }
  /**
    * Tests if intersection of two sets gives proper results.
    */
  private def testWBTreeSetIntersectionNotEmptySetsNotEmptyIntersection(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2).add(3)
    val mySet2 = WBTreeSet[Int]().add(1).add(7).add(3)
    assert(mySet1.intersect(mySet2).getAllElements.toSet == List(1, 3).toSet)
  }
  /**
    * Tests if intersection of two sets gives proper results.
    */
  private def testWBTreeSetIntersectionNotEmptySetsEmptyIntersection(): Unit = {
    val mySet1 = WBTreeSet[Int]().add(1).add(2).add(3)
    val mySet2 = WBTreeSet[Int]().add(4).add(5).add(6)
    assert(mySet1.intersect(mySet2).getAllElements == List())
  }

}
