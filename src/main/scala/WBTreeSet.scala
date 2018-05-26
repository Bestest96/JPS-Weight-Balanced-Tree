/**
  * A class representing the set based on weight-based tree.
  * @param tree A tree used to store data.
  * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
  * @tparam K A type of elements in a set.
  */
case class WBTreeSet[K](tree: Tree[K, Nothing])(implicit ord: K => Ordered[K]) {

  /**
    * Checks if a given element is in a set.
    * @param elem An element to be found.
    * @return True if element is found, false otherwise.
    */
  def contains(elem: K): Boolean = {
    if (tree.find(elem).isDefined) true else false
  }

  /**
    * Adds a new element to a set, returns the same set if it is already added.
    * @param elem An element to be added.
    * @return A new set with new element or the same set if the element has already existed.
    */
  def add(elem: K): WBTreeSet[K] = {
    if (contains(elem)) this else WBTreeSet(tree.add(elem))
  }

  /**
    * Deletes a given element, returns the same set if element to be deleted is not found.
    * @param elem An element to be deleted.
    * @return A new set with deleted element or the same set if element to be deleted is not found.
    */
  def delete(elem: K): WBTreeSet[K] = {
    if (tree.find(elem).isEmpty) this else WBTreeSet(tree.delete(elem))
  }

  /**
    * Creates a set which has a sum of two given sets.
    * @param other The second set to get elements to sum.
    * @return A new set containing the sum of two sets.
    */
  def sum(other: WBTreeSet[K]): WBTreeSet[K] = {
    WBTreeSet(TreeGenerator.generateKeys((tree.keys() ::: other.tree.keys()).distinct))
  }

  /**
    * Creates a set which contains an intersection of elements from two sets.
    * @param other The second set to get elements to intersect.
    * @return A new set containing the intersection of two sets.
    */
  def intersection(other: WBTreeSet[K]): WBTreeSet[K] = {
    WBTreeSet(TreeGenerator.generateKeys(tree.keys().intersect(other.tree.keys())))
  }

  /**
    * Returns all elements in a set.
    * @return A list of all elements in a set.
    */
  def getAllElements: List[K] = {
    tree.keys()
  }

  def size(): Int = if (tree.root.isDefined) tree.root.get.size else 0
}

/**
  * An object required for a WBTreeSet object to have a no-parameter constructor.
  */
object WBTreeSet {
  /**
    * An override of an apply method that takes no arguments.
    * @param ord Implicit argument specifying that a type K must be convertible to Ordered[K].
    * @tparam K A type of elements in a set.
    * @return A newly created set with an empty tree.
    */
  def apply[K]()(implicit ord: K => Ordered[K]): WBTreeSet[K] = WBTreeSet(Tree[K, Nothing]())
}
