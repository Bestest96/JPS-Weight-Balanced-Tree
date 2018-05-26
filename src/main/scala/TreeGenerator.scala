object TreeGenerator {
  def generatePairs[K, V](iterable: Iterable[(K, Option[V])])(implicit ord: K => Ordered[K]): Tree[K, V] = {
      val nodes = for ((key, value) <- iterable) yield Node(key, value)
      nodes.foldLeft(Tree[K, V]())((B: Tree[K, V], A: Node[K, V]) => B.add(A.key, A.value))
  }

  def generateKeys[K](iterable: Iterable[K])(implicit ord: K => Ordered[K]): Tree[K, Nothing] = {
    iterable.foldLeft(Tree[K, Nothing]())((B: Tree[K, Nothing], key: K) => B.add(key, None))
  }
}
