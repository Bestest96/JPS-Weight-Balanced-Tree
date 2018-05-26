/**
  * A main object of a program.
  */
object Main {
  /**
    * Main function.
    * @param args Arguments given through the console.
    */
  def main(args: Array[String]): Unit = {
//    Test.testAll()
    val test = WBTreeSetGenerator.generate(List(1, 2, 3, 4))
    println(test.getAllElements)
  }
}
