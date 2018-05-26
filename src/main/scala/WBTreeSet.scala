case class WBTreeSet[K](tree: Tree[K, Nothing])(implicit ord: K => Ordered[K]) {


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
    WBTreeSet(TreeGenerator.generateKeys((tree.keys() ::: other.tree.keys()).distinct))
  }

  def intersection(other: WBTreeSet[K]): WBTreeSet[K] = {
    WBTreeSet(TreeGenerator.generateKeys(tree.keys().intersect(other.tree.keys())))
  }

  def getAllElements: List[K] = {
    tree.keys()
  }
}

object WBTreeSet {
  def apply[K](implicit ord: K => Ordered[K]): WBTreeSet[K] = WBTreeSet(Tree[K, Nothing]())
}
