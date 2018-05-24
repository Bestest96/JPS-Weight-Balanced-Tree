import scala.annotation.tailrec

case class Tree[K, V](root: Option[Node[K, V]] = None, alpha: Double = 0.25)(implicit ord: K => Ordered[K]) {

  def add(key: K, value: Option[V] = None): Tree[K, V] = {
    if (root.isEmpty)
      Tree(Option(Node[K, V](key, value)))
    else
      this.copy(root = Option(addNode(key, value, root.get)))
  }

  private def addNode(key: K, value: Option[V] = None, node: Node[K, V]): Node[K, V] =
    if (node.key >= key) {
      if (node.left.isEmpty) balance(node.copy(left = Option(Node(key, value)), size = node.size + 1))
      else balance(node.copy(left = Option(addNode(key, value, node.left.get)), size = node.size + 1))
    }
    else {
      if (node.right.isEmpty) balance(node.copy(right = Option(Node(key, value)), size = node.size + 1))
      else balance(node.copy(right = Option(addNode(key, value, node.right.get)), size = node.size + 1))

    }

  //  def delete(key: Int): Tree = {
  //    val toDelete = find(key)
  //    if (toDelete.isEmpty)
  //      return this
  //    this.copy(root = Option(deleteNode(key, this.root.get, None)))
  //
  //
  //  }
  //
  //  private def deleteNode(key: Int, node: Node, parent: Option[Node]): Node = {
  //    if (node.key > key && node.left.isDefined)
  //      balance(node.copy(left = Option(deleteNode(key, node.left.get, Option(node))), size = node.size))
  //    else if (node.key < key && node.right.isDefined)
  //      balance(node.copy(right = Option(deleteNode(key, node.right.get, Option(node))), size = node.size))
  //    else if (node.key == key) {
  //      (node.left, node.right) match {
  //        case (None, None) => {
  //          val leftParentChild = if (parent.get.left.isDefined && parent.get.left.get == node) None else parent.get.left
  //          val rightParentChild = if (parent.get.right.isDefined && parent.get.right.get == node) None else parent.get.right
  //          balance(parent.get.copy(left = leftParentChild, right = rightParentChild, size = parent.size - 1))
  //        }
  //      }
  //    }
  //  }

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

  def rotateRight(startNode: Option[Node[K, V]]): Node[K, V] = {
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
    startNodeL.get.copy(right = Option(startNode.get.copy(left = startNodeLR, size = startNodeSize)), size = startNodeLSize)
  }

  def rotateLeft(startNode: Option[Node[K, V]]): Node[K, V] = {
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
    startNodeR.get.copy(left = Option(startNode.get.copy(right = startNodeRL, size = startNodeSize)), size = startNodeRSize)
  }

  def balance(node: Node[K, V]): Node[K, V] = {
    val leftWeight = node.left match {
      case None => 1
      case _ => node.left.get.weight()
    }
    val rightWeight = node.right match {
      case None => 1
      case _ => node.right.get.weight()
    }
    node.weight() match {
      case x if alpha * x > leftWeight => rotateLeft(Option(node))
      case x if alpha * x > rightWeight => rotateRight(Option(node))
      case _ => node
    }
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
        val currText = (node.get.key.toString, node.get.value.getOrElse("").toString).toString
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