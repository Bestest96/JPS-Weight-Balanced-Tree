object TreeGenerator {
  def generate[K, V](iterable: Iterable[(K, Option[V])])(implicit ord: K => Ordered[K]): Tree[K, V] = {
      val nodes = for ((key, value) <- iterable) yield Node(key, value)
      nodes.foldLeft(Tree[K, V]())((B: Tree[K, V], A: Node[K, V]) => B.add(A.key, A.value))
  }
}
