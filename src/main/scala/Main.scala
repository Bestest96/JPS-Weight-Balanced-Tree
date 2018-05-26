object Main {
  def main(args: Array[String]): Unit = {
//    Test.testAll()
    val test = WBTreeSetGenerator.generate(List(1, 2, 3, 4))
    println(test.getAllElements)
  }
}
