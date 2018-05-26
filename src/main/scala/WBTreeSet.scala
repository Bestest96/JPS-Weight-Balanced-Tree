case class WBTreeSet[K: Ordering](tree: Tree[K, Nothing]) {

  def contains(elem: K): Boolean = {
    if (tree.find(elem).isDefined) true else false
  }

  def add(elem: K): WBTreeSet[K] = {
    if (contains(elem)) this else WBTreeSet(tree.add(elem))
  }

  def delete(elem: K): WBTreeSet[K] = {
    if (tree.find(elem).isEmpty) this else WBTreeSet(tree.delete(elem))
  }

  def sum(other: WBTreeSet[K]): WBTreeSet[K] = {
    WBTreeSet(TreeGenerator.generateFromKeys((tree.keys() ::: other.tree.keys()).distinct))
  }

  def intersection(other: WBTreeSet[K]): WBTreeSet[K] = {
    WBTreeSet(TreeGenerator.generateFromKeys(tree.keys().intersect(other.tree.keys())))
  }

  def getAllElements: List[K] = {
    tree.keys()
  }

}

object WBTreeSet {
  def apply[K: Ordering](): WBTreeSet[K] = WBTreeSet(Tree[K, Nothing]())
}
