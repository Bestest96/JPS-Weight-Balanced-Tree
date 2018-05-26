/**
  * An object generating sets from a sequence of values.
  */
object WBTreeSetGenerator {
  /**
    * Generate a set from keys.
    * @param iterable An iterable collection of keys.
    * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
    * @tparam K A type of the key.
    * @return A generated set.
    */
  def generate[K](iterable: Iterable[K])(implicit ord: K => Ordered[K]): WBTreeSet[K] = {
    iterable.foldLeft(WBTreeSet[K]())((B: WBTreeSet[K], key: K) => B.add(key))
  }
}
