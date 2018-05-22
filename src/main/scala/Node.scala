case class Node(key: Int, left: Option[Node] = None, right: Option[Node] = None, size: Int = 1) {
  def weight(): Int = size + 1
}