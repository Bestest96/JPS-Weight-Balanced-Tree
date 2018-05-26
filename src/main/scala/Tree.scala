import scala.annotation.tailrec


/**
  * A class representing the weight-balanced tree.
  * @param root A node that is a root of a tree.
  * @param alpha A constant factor used when deciding about balancing the tree.
  * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
  * @tparam K A type of the key.
  * @tparam V A type of the value (may be Nothing if not used).
  */
case class Tree[K, V](root: Option[Node[K, V]] = None, alpha: Double = 0.25)(implicit ord: K => Ordered[K]) {

  /**
    * Gets keys from the tree and returns them as a list.
    * @return A list of all keys from a tree.
    */
  def keys(): List[K] = {
    /**
      * Recursively creates a list of keys.
      * @param node Actual node to take key from.
      * @return A list of collected keys.
      */
    def keyList(node: Option[Node[K, V]]): List[K] = {
      if (node.isEmpty)
        List.empty
      else
        node.get.key :: keyList(node.get.left) ::: keyList(node.get.right)
    }
    keyList(root)
  }

  /**
    * Adds a new node with a given key and value to the tree, balances the tree if needed.
    * @param key A key of a node to be added (used for determining the place in a tree).
    * @param value A value mapped to the node (default is None).
    * @return A new tree with a new node added or the same tree if the element is already in a tree.
    */
  def add(key: K, value: Option[V] = None): Tree[K, V] = {
    val toAdd = Node[K, V](key, value)
    if (root.isEmpty)
      Tree(Option(toAdd))
    else {
      val foundNode = find(key)
      if (foundNode.isDefined) {
        if (foundNode.get.value == value)
          this
        else
          this.delete(key).add(key, value)
      }
      else this.copy(root = Option(addNode(toAdd, root.get)._2))
    }
  }

  /**
    * Helper function for add function.
    * @param nodeToAdd A node to be added to the tree.
    * @param node A node to be checked for adding a new element.
    * @return A tuple containing an int representing the branch we chose (0 - left, 1 - right) and a chosen node.
    */
  private def addNode(nodeToAdd: Node[K, V], node: Node[K, V] = root.get): (Int, Node[K, V]) =
    if (node.key >= nodeToAdd.key) {
      if (node.left.isEmpty) (0, node.copy(left = Option(nodeToAdd), size = node.size + 1))
      else{
        val (nextDirection, newSon) = addNode(nodeToAdd, node.left.get)
        val newNode = node.copy(left = Option(newSon), size = node.size + 1)
        if (isBalanced(newNode)) (0, newNode)
        else (0, balance(newNode, 0, nextDirection))
      }
    }
    else {
      if (node.right.isEmpty) (1, node.copy(right = Option(nodeToAdd), size = node.size + 1))
      else{
        val (nextDirection, newSon) = addNode(nodeToAdd, node.right.get)
        val newNode = node.copy(right = Option(newSon), size = node.size + 1)
        if (isBalanced(newNode)) (1, newNode)
        else (1, balance(newNode, 1, nextDirection))
      }
    }

  /**
    * Deletes a node with the given key from a tree, returns the same tree if element not found.
    * @param key A key for a node to be deleted.
    * @return A new tree without a deleted node or the same tree if element not found.
    */
  final def delete(key: K): Tree[K, V] = {
    val toDelete = find(key)
    if (toDelete.isEmpty)
      this
    else
      this.copy(root = helperDelete(toDelete.get, root))
  }

//  /**
//    * Deletes the given node, returns the same tree if a node with given key is not found
//    * @param toDelete A node to be deleted.
//    * @return A new tree without all deleted nodes or the same tree if a node with given key is not found.
//    */
//  def deleteNode(toDelete: Node[K, V]): Tree[K, V] = {
//    find(toDelete.key) match {
//      case None => this
//      case _ => this.copy(root = helperDelete(toDelete, root))
//    }
//  }

  /**
    * A helper function for delete function.
    * @param toDelete A node to be deleted.
    * @param currNode Currently checked node.
    * @return Some with new node with deleted element or None.
    */
  private def helperDelete(toDelete: Node[K, V], currNode: Option[Node[K, V]]): Option[Node[K, V]] = {
    if (currNode.isEmpty) return currNode
    // found the node to delete
    val newNode =
      if (currNode.get == toDelete) {
        val newCurrNode =
          // the current node has at least one non-empty child
          if (currNode.get.left.isEmpty || currNode.get.right.isEmpty) {
            if (currNode.get.size == 1) None
            else Option(currNode.get.left.getOrElse(currNode.get.right.get))
          }
          // the current node has both children
          else{
            val rightSubtreeMinNode = getMinNode(currNode.get.right.get)
            val substitute = rightSubtreeMinNode.copy(left = currNode.get.left, right = currNode.get.right, size = currNode.get.size)
            Option(substitute.copy(right = helperDelete(rightSubtreeMinNode, substitute.right), size = substitute.size - 1))
          }

        newCurrNode
      }
      // the node to delete not found yet
      else {
        // the node to delete is in the left subtree
        val newNode =
          if (toDelete.key <= currNode.get.key){
            val newSon = helperDelete(toDelete, currNode.get.left)
            val newNode = currNode.get.copy(left = newSon, size = currNode.get.size - 1)
            newNode
          }
          // the node to delete is in the right subtree
          else{
            val newSon = helperDelete(toDelete, currNode.get.right)
            val newNode = currNode.get.copy(right = newSon, size = currNode.get.size - 1)
            newNode
          }

        Option(newNode)
      }

    newNode match {
      case None => newNode
      case _ =>
        if (isBalanced(newNode.get)) newNode
        else{
          val (side1, child1) = getChildWithGreaterWeight(newNode.get)
          val (side2, _) = getChildWithGreaterWeight(child1)
          Option(balance(newNode.get, side1, side2))
        }
    }
  }

  /**
    * Gets the node with minimum key.
    * @param node A starting node.
    * @return A node with the minimum key.
    */
  private def getMinNode(node: Node[K, V]): Node[K, V] = {
    /**
      * Recursively gets the node with minimum key.
      * @param node Actually checked node.
      * @return A node with less key or the same node (if nothing less is found).
      */
    @tailrec
    def inner(node: Node[K, V]): Node[K, V] =
      if (node.left.isEmpty) node
      else inner(node.left.get)

    inner(node)
  }

  /**
    * Gets the first child with a greater weight.
    * @param node A staring node.
    * @return A tuple containing an int representing the branch we chose (0 - left, 1 - right) and a chosen node.
    */
  private def getChildWithGreaterWeight(node: Node[K, V]): (Int, Node[K, V]) = {
    (node.left, node.right) match {
      case (x: Some[Node[K, V]], y: Some[Node[K, V]]) if x.get.weight() >= y.get.weight() => (0, x.get)
      case (x: Some[Node[K, V]], y: Some[Node[K, V]]) if x.get.weight() < y.get.weight() => (1, y.get)
      case (_, x: Some[Node[K, V]]) => (1, x.get)
      case (x: Some[Node[K, V]], _) => (0, x.get)
      case _ => throw new IllegalArgumentException("Expected non-leaf node.")
    }
  }

  /**
    * Checks if a tree in a given place is balanced.
    * @param node A root of checked tree.
    * @return True if a tree is balanced, false otherwise.
    */
  private def isBalanced(node: Node[K, V]): Boolean = {
    val leftWeight = node.left match {
      case None => 1
      case _ => node.left.get.weight()
    }
    val rightWeight = node.right match {
      case None => 1
      case _ => node.right.get.weight()
    }
    node.weight() match {
      case x if alpha * x > leftWeight => false
      case x if alpha * x > rightWeight => false
      case _ => true
    }
  }

  /**
    * Checks if a node and its subtrees are balanced.
    * @param node A node to check.
    * @return True if balanced, false otherwise.
    */
  def isSubtreeBalanced(node: Option[Node[K, V]]): Boolean = {
    if (node.isEmpty) true
    else isBalanced(node.get) && isSubtreeBalanced(node.get.left) && isSubtreeBalanced(node.get.right)
  }

  /**
    * Chceck if a whole tree is balanced.
    * @return True if balanced, false otherwise.
    */
  def isTreeBalanced: Boolean = isSubtreeBalanced(root)

  /**
    * Balances the tree at a given node.
    * @param node Node at which we perform balancing.
    * @param first First branch we chose during adding or deleting the node. (0 - left, 1 - right)
    * @param second Second branch we chose during adding or deleting the node. (0 - left, 1 - right)
    * @return Node with performed balance operations.
    */
  private def balance(node: Node[K, V], first: Int, second: Int): Node[K, V] = (first, second) match {
    case (x: Int, y: Int) if x == 0 && y == 0 => rotateRight(Option(node)).get
    case (x: Int, y: Int) if x == 0 && y == 1 => rotateRight(Option(node.copy(left = rotateLeft(node.left)))).get
    case (x: Int, y: Int) if x == 1 && y == 0 => rotateLeft(Option(node.copy(right = rotateRight(node.right)))).get
    case (x: Int, y: Int) if x == 1 && y == 1 => rotateLeft(Option(node)).get
    case _ => throw new IllegalArgumentException ("first, second arguments should be from range [0, 1]")
  }

  /**
    * Finds the FIRST node with a given key in a tree.
    * @param key A key of a node to be found.
    * @return Some with a found node or None if not found.
    */
  def find(key: K): Option[Node[K, V]] = {

    /**
      * Recursively searches a tree for a given element.
      * @param key A key of a node to be found.
      * @param node An actually checked node.
      * @return Some with a next node or None if empty.
      */
    @tailrec
    def searchInTree(key: K, node: Option[Node[K, V]]): Option[Node[K, V]] = {
      if (node.isEmpty)
        None
      else if (node.get.key > key) {
        searchInTree(key, node.get.left)
      } else {
        if (node.get.key == key) {
          node
        } else {
          searchInTree(key, node.get.right)
        }
      }
    }

    searchInTree(key, root)
  }

  /**
    * Performs a right rotation at a given node.
    * @param startNode A node at which we are rotating.
    * @return A new node after applying a rotation.
    */
  def rotateRight(startNode: Option[Node[K, V]]): Option[Node[K, V]] = {
    val startNodeL = startNode.get.left
    val startNodeLR = startNodeL.get.right
    val startNodeLRSize = startNodeLR match {
      case None => 0
      case _ => startNodeLR.get.size
    }
    val startNodeSize = startNode.get.right match {
      case None => startNodeLRSize + 1
      case _ => startNodeLRSize + startNode.get.right.get.size + 1
    }
    val startNodeLSize = startNodeL.get.left match {
      case None => startNodeSize + 1
      case _ => startNodeSize + startNodeL.get.left.get.size + 1
    }
    Option(startNodeL.get.copy(right = Option(startNode.get.copy(left = startNodeLR, size = startNodeSize)), size = startNodeLSize))
  }

  /**
    * Performs a left rotation at a given node.
    * @param startNode A node at which we are rotating.
    * @return A new node after applying a rotation.
    */
  def rotateLeft(startNode: Option[Node[K, V]]): Option[Node[K, V]] = {
    val startNodeR = startNode.get.right
    val startNodeRL = startNodeR.get.left
    val startNodeRLSize = startNodeRL match {
      case None => 0
      case _ => startNodeRL.get.size
    }
    val startNodeSize = startNode.get.left match {
      case None => startNodeRLSize + 1
      case _ => startNodeRLSize + startNode.get.left.get.size + 1
    }
    val startNodeRSize = startNodeR.get.right match {
      case None => startNodeSize + 1
      case _ => startNodeSize + startNodeR.get.right.get.size + 1
    }
    Option(startNodeR.get.copy(left = Option(startNode.get.copy(right = startNodeRL, size = startNodeSize)), size = startNodeRSize))
  }

  /**
    * Prints elements of a tree in order.
    */
  def printInOrder(): Unit = {
    if (root.get.left.isDefined) Tree(root.get.left).printInOrder()
    println("key: " + root.get.key + ", value: " + root.get.value.getOrElse("None"))
    if (root.get.right.isDefined) Tree(root.get.right).printInOrder()
  }

  /**
    * Prints elements in a BFS order.
    */
  def printBfs(): Unit = {
    /**
      * Creates a sequence to be printed.
      * @param s1 A stream of nodes from the actual level.
      * @param s2 A stream of nodes from the next level.
      * @param output A sequence of already visited nodes.
      * @param f A function that converted a node into a stream.
      * @return A sequence of nodes to be printed.
      */
    def recursive(s1: Stream[Node[K, V]], s2: Stream[Node[K, V]],
                  output: Seq[Stream[Node[K, V]]], f: Node[K, V] => Stream[Node[K, V]]): Seq[Stream[Node[K, V]]] = {
      (s1, s2) match {
        case (x1: Stream[Node[K, V]], x2: Stream[Node[K, V]]) if x1.isEmpty && x2.isEmpty => output
        case (x1: Stream[Node[K, V]], x2: Stream[Node[K, V]]) if x1.isEmpty => recursive(x2, Stream(), output :+ x2, f)
        case _ => recursive(s1.tail, s2 append f(s1.head), output, f)
      }
    }

    /**
      * Filters none childer from a node.
      * @param node A node to be filtered.
      * @return A stream with children nodes that were not None.
      */
    def filterNoneChildren(node: Node[K, V]): Stream[Node[K, V]] = {
      val filteredChildren = Stream(node.left, node.right).filter(_.isDefined).map(_.get)
      filteredChildren
    }

    if (root.isDefined) {
      val output = recursive(Stream(), Stream(root.get), Seq(), filterNoneChildren)
      for (s: Stream[Node[K, V]] <- output) {
        s.map((node: Node[K, V]) => (node.key, node.value)).foreach((x: (K, Option[V])) => {
          print(x._1, x._2.get)
          print(" ")
        })
        println()
      }
    }
  }

  /**
    * Creates a frame for a prettyBfs print
    * @param x A sequence of elements to be printed in a frame.
    * @param verticalChar A vertical character of a frame.
    * @param horizontalChar A horizontal character of a frame.
    * @param corner1Char An upper left corner character.
    * @param corner2Char An upper right corner character.
    * @param corner3Char A lower right corner character.
    * @param corner4Char A lower left corner character.
    * @return A sequence to be prented.
    */
  private def frame(x: Seq[String], verticalChar: String = "|", horizontalChar: String = "-",
                    corner1Char: String = "x", corner2Char: String = "x",
                    corner3Char: String = "x", corner4Char: String = "x"): Seq[String] = {
    val width = x.head.length
    val height = x.length

    val outer = Seq(corner1Char + horizontalChar * width + corner2Char,
      corner3Char + horizontalChar * width + corner4Char)
    val inner = for(row <- x) yield verticalChar + row + verticalChar
    outer.head +: inner :+ outer(1)
  }

  /**
    * Creates an empty frame.
    * @param x A sequence of elements to be printed in a frame.
    * @return A sequence to be prented.
    */
  private def emptyFrame(x: Seq[String]): Seq[String] = {
    frame(x, " ", " ", " ", " ", " ", " ")
  }

  /**
    * Merges two frames so that they fit each other and adds some space based on margin.
    * @param leftDraw A left part of a created frame.
    * @param rightDraw A right part of a created frame.
    * @param leftMargin A margin to be added to the right.
    * @param rightMargin A margin to be added to the left.
    * @return A sequence of merged frames.
    */
  private def mergeDraws(leftDraw: Seq[String], rightDraw: Seq[String], leftMargin: Int = 0, rightMargin: Int = 0): Seq[String] = {
    val leftWidth = if (leftDraw.isEmpty) 0 else leftDraw.head.length
    val rightWidth = if (rightDraw.isEmpty) 0 else rightDraw.head.length
    val leftHeight = leftDraw.length
    val rightHeight = rightDraw.length
    val currHeight = math.max(leftHeight, rightHeight)

    val suffix1 = for(_ <- 1 to currHeight - leftHeight) yield " " * leftWidth
    val suffix2 = for(_ <- 1 to currHeight - rightHeight) yield " " * rightWidth
    val newLeftDraw = leftDraw ++ suffix1
    val newRightDraw = rightDraw ++ suffix2

    for( i <- 0 until currHeight ) yield " " * leftMargin + newLeftDraw(i) + newRightDraw(i) + " " * rightMargin
  }

  /**
    * Prints a tree in a bfs order with some fancy frames.
    * @param node A starting node.
    */
  def prettyPrintBfs(node: Option[Node[K, V]] = root): Unit = {
    /**
      * Draws a representation of a node.
      * @param node A node to draw.
      * @return A sequence to be printed.
      */
    def draw(node: Option[Node[K, V]]): Seq[String] = {
      if (node.isEmpty) Seq()
      else {
        val lDraw = draw(node.get.left)
        val rDraw = draw(node.get.right)
        val lWidth = if (lDraw.isEmpty) 0 else lDraw.head.length()
        val rWidth = if (rDraw.isEmpty) 0 else rDraw.head.length()
        val currText = (node.get.key.toString, node.get.value.getOrElse("None").toString, node.get.size.toString).toString
        val textLength = currText.length

        val leftHalfTextLength = (0.5*textLength + 1).toInt
        val rightHalfTextLength = textLength - leftHalfTextLength
        val middleLinePos = math.max(lWidth, leftHalfTextLength)

        val leftOffset = middleLinePos - leftHalfTextLength
        val rightOffset = math.max(rightHalfTextLength, rWidth) - rightHalfTextLength
        val currString = " " * leftOffset + currText + " " * rightOffset

        val leftMargin = middleLinePos - lWidth
        val rightMargin = if(rightHalfTextLength > rWidth) rightHalfTextLength - rWidth else 0
        val newDraw = currString +: mergeDraws(leftDraw = lDraw, rightDraw = rDraw,
          leftMargin = leftMargin, rightMargin = rightMargin)

        frame(emptyFrame(newDraw))
      }
   }

    draw(node).foreach(println)
  }
}