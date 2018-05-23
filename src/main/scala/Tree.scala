import scala.annotation.tailrec

case class Tree(root: Option[Node] = None, alpha: Double = 0.25) {

  def add(elem: Int): Tree = {
    if (root.isEmpty)
      Tree(Option(Node(elem)))
    else
      this.copy(root = Option(addNode(elem, root.get)))
  }

  private def addNode(elem: Int, node: Node): Node =
    if (node.key >= elem) {
      if (node.left.isEmpty) balance(node.copy(left = Option(Node(elem)), size = node.size + 1))
      else balance(node.copy(left = Option(addNode(elem, node.left.get)), size = node.size + 1))
    }
    else {
      if (node.right.isEmpty) balance(node.copy(right = Option(Node(elem)), size = node.size + 1))
      else balance(node.copy(right = Option(addNode(elem, node.right.get)), size = node.size + 1))
    }

//  def delete(elem: Int): Tree = {
//    val toDelete = find(elem)
//    if (toDelete.isEmpty)
//      return this
//    this.copy(root = Option(deleteNode(elem, this.root.get, None)))
//
//
//  }
//
//  private def deleteNode(elem: Int, node: Node, parent: Option[Node]): Node = {
//    if (node.key > elem && node.left.isDefined)
//      balance(node.copy(left = Option(deleteNode(elem, node.left.get, Option(node))), size = node.size))
//    else if (node.key < elem && node.right.isDefined)
//      balance(node.copy(right = Option(deleteNode(elem, node.right.get, Option(node))), size = node.size))
//    else if (node.key == elem) {
//      (node.left, node.right) match {
//        case (None, None) => {
//          val leftParentChild = if (parent.get.left.isDefined && parent.get.left.get == node) None else parent.get.left
//          val rightParentChild = if (parent.get.right.isDefined && parent.get.right.get == node) None else parent.get.right
//          balance(parent.get.copy(left = leftParentChild, right = rightParentChild, size = parent.size - 1))
//        }
//      }
//    }
//  }

  def find(elem: Int): Option[Node] = {

    @tailrec
    def searchInTree(elem: Int, node: Option[Node]): Option[Node] = {
      if (node.isEmpty)
        None
      else node.get.key - elem match {
        case 0 => node
        case x if x > 0 => searchInTree(elem, node.get.left)
        case x if x < 0 => searchInTree(elem, node.get.right)
      }
    }
    searchInTree(elem, root)
  }

  def rotateRight(startNode: Option[Node]): Node = {
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

  def rotateLeft(startNode: Option[Node]): Node = {
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

  def balance(node: Node): Node = {
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
    println(root.get.key)
    if (root.get.right.isDefined) Tree(root.get.right).printInOrder()
  }

  def printBfs(): Unit = {
    def recursive(s1: Stream[Node], s2: Stream[Node],
                  output: Seq[Stream[Node]], f: Node => Stream[Node]): Seq[Stream[Node]] = {
      (s1, s2) match {
        case (x1: Stream[Node], x2: Stream[Node]) if x1.isEmpty && x2.isEmpty => output
        case (x1: Stream[Node], x2: Stream[Node]) if x1.isEmpty => recursive(x2, Stream(), output :+ x2, f)
        case _ => recursive(s1.tail, s2 append f(s1.head), output, f)
      }
    }

    def filterNoneChildren(node: Node): Stream[Node] = {
      val filteredChildren = Stream(node.left, node.right).filter(_.isDefined).map(_.get)
      filteredChildren
    }

    if (root.isDefined) {
      val output = recursive(Stream(), Stream(root.get), Seq(), filterNoneChildren)
      for(s: Stream[Node] <- output){
        s.map(_.key).foreach( (x: Int) => { print(x); print(" ") })
        println()
      }
    }
  }
}
