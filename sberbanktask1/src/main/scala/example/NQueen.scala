package example
// sbt "runMain example.NQueen"

object NQueen extends App {
  type Queen = (Int, Int)
  type Solutions = List[List[Queen]]

  run()

  def run() = {
    val size = args(0).toInt * args(1).toInt
//    val n = 8; val m = 8
//    val size = n * m

    val solutions = placeQueens(size, size)
    println(solutions.size + " solutions found")

//    printResult(solutions.head, size)
    printAllSolutions(solutions, size)
  }

  // print the board of the first solution
  def printAllSolutions(solutions: Solutions, size: Int): Unit = {
    for(i <- solutions.indices) {
      printResult(solutions(i), size)
      if(i % 4 == 0)
        println("\n\n")
    }
  }

  def printResult(solution: List[Queen], size: Int): Unit = {
    for (queen <- solution; x <- 1 to size) {
      if (queen._2 == x) print("Q ") else print(". ")
      if (x == size) println()
    }
  }

  //  def printResult2(solutions: Solutions, size: Int): Unit = {
//    for (queen <- solutions.head; x <- 1 to size) {
//      if (queen._2 == x) print("Q ") else print(". ")
//      if (x == size) println()
//    }
//  }

  def placeQueens(n: Int, size: Int): Solutions = n match {
    case 0 => List(Nil)
    case _ => for {
      queens <- placeQueens(n - 1, size); y <- 1 to size
      queen = (n, y)
      if isSafe(queen, queens)
    } yield queen :: queens
  }

  def isSafe(queen: Queen, others: List[Queen]): Boolean =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: Queen, q2: Queen): Boolean =
    q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q2._1-q1._1).abs == (q2._2-q1._2).abs
}