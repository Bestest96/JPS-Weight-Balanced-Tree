/**
  * An object generating trees from a sequence of values.
  */
object TreeGenerator {
  /**
    * Generate a tree from key-value pairs.
    * @param iterable An iterable collection of key-value pairs.
    * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
    * @tparam K A type of the key.
    * @tparam V A type of the value.
    * @return A generated tree.
    */
  def generatePairs[K, V](iterable: Iterable[(K, Option[V])])(implicit ord: K => Ordered[K]): Tree[K, V] = {
      val nodes = for ((key, value) <- iterable) yield Node(key, value)
      nodes.foldLeft(Tree[K, V]())((B: Tree[K, V], A: Node[K, V]) => B.add(A.key, A.value))
  }

  /**
    * Generate a tree from keys (without values).
    * @param iterable An iterable collection of keys.
    * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
    * @tparam K A type of the key.
    * @return A generated tree.
    */
  def generateKeys[K](iterable: Iterable[K])(implicit ord: K => Ordered[K]): Tree[K, Nothing] = {
    iterable.foldLeft(Tree[K, Nothing]())((B: Tree[K, Nothing], key: K) => B.add(key))
  }
}
