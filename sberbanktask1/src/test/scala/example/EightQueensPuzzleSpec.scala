package example

import example.{Coordinate => C}
//import example.Board._

import example.Rook
import org.scalatest._

import scala.collection.mutable

class EightQueensPuzzleSpec extends FlatSpec with Matchers {


//  "TEightQueensPuzzle posCount2" should "equals 5" in {
//    EightQueensPuzzle.posCount2() shouldEqual 5
//  }

  val knightIn5_5_PosibPlaces = mutable.ArrayBuffer(
    C(3,4), C(4,3), C(4,1), C(3,0),
    C(1,0), C(0,1), C(0,3), C(1,4)
  )

  val queenIn5_5_PosibPlaces = mutable.ArrayBuffer(
    C(0,0), C(2,0), C(4,0),
    C(1,1), C(2,1), C(3,1),
    C(0,2), C(1,2), C(3,2), C(4,2),
    C(1,3), C(2,3), C(3,3),
    C(0,4), C(2,4), C(4,4)
  )

  val rookIn5x5_PosibPlaces = mutable.ArrayBuffer(
    C(2,0), C(2,1), C(2,3), C(2,4), // UpD
    C(0,2), C(1,2), C(3,2), C(4,2) // LeftRight
  )

  val bishopIn5x5_PosibPlaces = mutable.ArrayBuffer(
    C(3,3), C(4,4),
    C(3,1), C(4,0),
    C(1,1), C(0,0),
    C(1,3), C(0,4)
  )

  "knightIn5_5_PosibPlaces" should "equal answers" in {
    arraySorted(Utils.allKnightCells(5, 5, C(2,2))) shouldEqual arraySorted(knightIn5_5_PosibPlaces)
  }

  "THISArrays: Queens places == Rook ++ Bishop places" should "equal answers" in {
    arraySorted(queenIn5_5_PosibPlaces) shouldEqual arraySorted(rookIn5x5_PosibPlaces ++ bishopIn5x5_PosibPlaces)
  }

  "Rook In 5_5_PosibPlaces" should "equal answers" in {
    arraySorted(Utils.allLeftRightUpDown(5, 5, C(2,2))) shouldEqual arraySorted(rookIn5x5_PosibPlaces)
  }

  "Bishop In 5_5_PosibPlaces" should "equal answers" in {
    arraySorted(Utils.allDiagsFrom(5, 5, C(2,2))) shouldEqual arraySorted(bishopIn5x5_PosibPlaces)
  }

  "Queens places == Rook ++ Bishop places" should "equal answers" in {
    arraySorted(
      Utils.allLeftRightUpDown(5, 5, C(2,2)) ++ Utils.allDiagsFrom(5, 5, C(2,2))
    ) shouldEqual arraySorted(rookIn5x5_PosibPlaces ++ bishopIn5x5_PosibPlaces)
  }

  "Queens at 5x5 Positions" should "equal answers" in {
    arraySorted(
      Utils.allLeftRightUpDown(5, 5, C(2,2)) ++ Utils.allDiagsFrom(5, 5, C(2,2))
    ) shouldEqual arraySorted(queenIn5_5_PosibPlaces)
  }

  /*"8 Queens at 8 x 8" should "92 positions" in {
    val board: Board = new Board(8,8)
    EightQueensPuzzle.posCount(8, 8, List(board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen, board.Queen)) shouldEqual  92
  }*/

//  "3 Rooks at 3 x 3" should "DONT KNOW" in {
//    val board: Board = new Board(3,3)
//    EightQueensPuzzle.posCount(3, 3, List(Rook(3, 3), Rook(3, 3), Rook(3, 3))) shouldEqual 3
//  }


  /*"2 Kings, Queen, Rook, Knight, Bishop at board 6 x 9" should " 20136752 positions" in {
    EightQueensPuzzle.posCount(6,9,List(Kings, Kings, Queen, Rook, Knight, Bishop)) shouldEqual 20136752
  }*/

  def arraySorted(arr: mutable.ArrayBuffer[C]): mutable.ArrayBuffer[C] =  {
    arr.sortBy{case C(x,y) => x}.sortBy{case C(x,y) => y}
  }

}
