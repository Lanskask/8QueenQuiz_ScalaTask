package example

import example.{Coordinate => C}
import example.Board._

import org.scalatest._

import scala.collection.mutable

class EightQueensPuzzleSpec extends FlatSpec with Matchers {


//  "TEightQueensPuzzle posCount2" should "equals 5" in {
//    EightQueensPuzzle.posCount2() shouldEqual 5
//  }

  val knightIn5_5_PosibPlaces = mutable.ArraySeq(
    C(3,4), C(4,3), C(4,1), C(3,0),
    C(1,0), C(0,1), C(0,3), C(1,4)
  ).sortBy{case C(x, y) => x}

  "knightIn5_5_PosibPlaces" should "equal answers" in {
    val board: Board = new Board(5,5)
//    board.allKnightCells(Coordinate(2,2)).map(x => println(x))
    board.allKnightCells(C(2,2)).sortBy{case C(x,y) => x} shouldEqual knightIn5_5_PosibPlaces
    /*board.allKnightCells(Coordinate(2,2)).map{case Coordinate(x,y) => x}.sorted*/
  }

  "8 Queens at 8 x 8" should "92 positions" in {
    val board: Board = new Board(8,8)
    EightQueensPuzzle.posCount(8, 8, List(board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen)) shouldEqual  92
  }

  "2 Kings, Queen, Rook, Knight, Bishop at board 6 x 9" should " 20136752 positions" in {
    EightQueensPuzzle.posCount(6,9,List(Kings, Kings, Queen, Rook, Knight, Bishop)) shouldEqual 20136752
  }

}
