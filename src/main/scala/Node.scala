/**
  * A class representing a node in a tree.
  * @param key A value used to place a node in a tree (to maintain order).
  * @param value A value mapped by the key value.
  * @param left A node containing the left subtree of this node.
  * @param right A node containing the right subtree of this node.
  * @param size The size of the node, defined as right.size + left.size + 1.
  * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
  * @tparam K A type of the key.
  * @tparam V A type of the value (may be Nothing if not used).
  */

case class Node[K, V](key: K, value: Option[V] = None, left: Option[Node[K, V]] = None, right: Option[Node[K, V]] = None, size: Int = 1)(implicit ord: K => Ordered[K]) {
  /**
    * Returns the weight of a node equal to its size incremented by one.
    * @return the weight of a node
    */
  def weight(): Int = size + 1

  /**
    * Creates and returns a string representation of a node.
    * @return a string representation of a node.
    */
  override def toString: String = (key, value.getOrElse("")).toString()
}