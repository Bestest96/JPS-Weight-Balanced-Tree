object WBTreeSetGenerator {
  def generate[K](iterable: Iterable[K]): WBTreeSet[K] = {
    iterable.foldLeft(WBTreeSet[K]())((B: WBTreeSet[K], key: K) => B.add(key))
  }
}
