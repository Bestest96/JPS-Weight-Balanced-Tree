case class Node[K, V](key: K, value: Option[V] = None, left: Option[Node[K, V]] = None, right: Option[Node[K, V]] = None, size: Int = 1)(implicit ord: K => Ordered[K]) {
  def weight(): Int = size + 1
}