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
    * @return a list of keys
    */
  def keys(): List[K] = {

    def keyList(node: Option[Node[K, V]]): List[K] = {
      if (node.isEmpty)
        List.empty
      else
        node.get.key :: keyList(node.get.left) ::: keyList(node.get.right)
    }
    keyList(root)
  }

  def add(key: K, value: Option[V] = None): Tree[K, V] = {
    val toAdd = Node[K, V](key, value)
    if (root.isEmpty)
      Tree(Option(toAdd))
    else
      this.copy(root = Option(addNode(toAdd, root.get)._2))
  }

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

  def delete(key: K): Tree[K, V] = {
    val toDelete = find(key)
    if (toDelete.isEmpty) this else this.copy(root = helperDelete(toDelete.get, root))
  }

  def deleteNode(toDelete: Node[K, V]): Tree[K, V] = {
    find(toDelete.key) match {
      case None => this
      case _ => this.copy(root = helperDelete(toDelete, root))
    }
  }

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

  private def getMinNode(node: Node[K, V]): Node[K, V] = {
    def inner(node: Node[K, V]): Node[K, V] =
      if (node.left.isEmpty) node
      else inner(node.left.get)

    inner(node)
  }

  private def getChildWithGreaterWeight(node: Node[K, V]): (Int, Node[K, V]) = {
    (node.left, node.right) match {
      case (x: Some[Node[K, V]], y: Some[Node[K, V]]) if x.get.weight() >= y.get.weight() => (0, x.get)
      case (x: Some[Node[K, V]], y: Some[Node[K, V]]) if x.get.weight() < y.get.weight() => (1, y.get)
      case (_, x: Some[Node[K, V]]) => (1, x.get)
      case (x: Some[Node[K, V]], _) => (0, x.get)
      case _ => throw new IllegalArgumentException("Expected non-leaf node.")
    }
  }

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

  def isSubtreeBalanced(node: Option[Node[K, V]]): Boolean = {
    if (node.isEmpty) true
    else isBalanced(node.get) && isSubtreeBalanced(node.get.left) && isSubtreeBalanced(node.get.right)
  }

  def isTreeBalanced: Boolean = isSubtreeBalanced(root)

  private def balance(node: Node[K, V], first: Int, second: Int): Node[K, V] = (first, second) match {
    case (x: Int, y: Int) if x == 0 && y == 0 => rotateRight(Option(node)).get
    case (x: Int, y: Int) if x == 0 && y == 1 => rotateRight(Option(node.copy(left = rotateLeft(node.left)))).get
    case (x: Int, y: Int) if x == 1 && y == 0 => rotateLeft(Option(node.copy(right = rotateRight(node.right)))).get
    case (x: Int, y: Int) if x == 1 && y == 1 => rotateLeft(Option(node)).get
    case _ => throw new IllegalArgumentException ("first, second arguments should be from range [0, 1]")
  }

  def find(key: K): Option[Node[K, V]] = {

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

  def printInOrder(): Unit = {
    if (root.get.left.isDefined) Tree(root.get.left).printInOrder()
    println("key: " + root.get.key + ", value: " + root.get.value.getOrElse("None"))
    if (root.get.right.isDefined) Tree(root.get.right).printInOrder()
  }

  def printBfs(): Unit = {
    def recursive(s1: Stream[Node[K, V]], s2: Stream[Node[K, V]],
                  output: Seq[Stream[Node[K, V]]], f: Node[K, V] => Stream[Node[K, V]]): Seq[Stream[Node[K, V]]] = {
      (s1, s2) match {
        case (x1: Stream[Node[K, V]], x2: Stream[Node[K, V]]) if x1.isEmpty && x2.isEmpty => output
        case (x1: Stream[Node[K, V]], x2: Stream[Node[K, V]]) if x1.isEmpty => recursive(x2, Stream(), output :+ x2, f)
        case _ => recursive(s1.tail, s2 append f(s1.head), output, f)
      }
    }

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

  private def emptyFrame(x: Seq[String]): Seq[String] = {
    frame(x, " ", " ", " ", " ", " ", " ")
  }

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

  def prettyPrintBfs(node: Option[Node[K, V]] = root): Unit = {
    def draw(node: Option[Node[K, V]], toFrame: Boolean = true): Seq[String] = {
      if (node.isEmpty) Seq()
      else {
        val lDraw = draw(node.get.left)
        val rDraw = draw(node.get.right)
        val lWidth = if (lDraw.isEmpty) 0 else lDraw.head.length()
        val rWidth = if (rDraw.isEmpty) 0 else rDraw.head.length()
//        val currText = (node.get.key.toString, node.get.value.getOrElse("").toString).toString
        val currText = (node.get.key.toString, node.get.size.toString).toString
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

        if (toFrame) frame(emptyFrame(newDraw)) else newDraw
      }
   }

    draw(node).foreach(println)
  }
}