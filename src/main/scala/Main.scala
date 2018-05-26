/**
  * A main object of a program.
  */
object Main {
  /**
    * Main function.
    * @param args Arguments given through the console.
    */
  def main(args: Array[String]): Unit = {
    val elements1 = Stream.from(1).take(100000)
    val set1 = elements1.foldLeft(WBTreeSet[Int]())((set: WBTreeSet[Int], key: Int) => set.add(key))
    val elements2 = Stream.from(100001).take(100000)
    val set2 = elements2.foldLeft(WBTreeSet[Int]())((set: WBTreeSet[Int], key: Int) => set.add(key))
    val setSum = set1.sum(set2)
    val setInt = set1.intersection(set2)
    println(setSum.size())
    println(setInt.size())
  }
}
