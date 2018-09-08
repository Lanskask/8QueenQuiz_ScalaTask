package example

import scala.collection.mutable

import example.{Coordinate => C}

case class Coordinate(x: Int, y: Int) {
  require(x >= 0)
  require(y >= 0)
}

sealed trait Figure {
  val priority: Int
  var coord: C

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C]
}

case class Rook(n: Int, m: Int) extends Figure {
  val priority: Int = 1
  var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allLeftRightUpDown(n, m, coord)
  }
}



class Board(_n: Int, _m: Int) {
  val (n, m) = (_n, _m)

  case class Queen(priority: Int = 1, var coord: C = C(0, 0)) extends Figure {
//    override val priority: Int = 1
//    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
      Utils.allDiagsFrom(n, m, coord) ++ Utils.allLeftRightUpDown(n, m, coord)
    }
  }

  class Rook extends Figure { // Башня
    override val priority: Int = 2
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
      Utils.allLeftRightUpDown(n, m, coord)
    }
  }

  class Bishop extends Figure { // Слон
    override val priority = 3
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
      Utils.allDiagsFrom(n, m, coord)
    }
  }

  class Knight extends Figure { // Конь
    override val priority = 4
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
      Utils.allKingCells(n, m, coord)
    }
  }

  class King extends Figure {
    override val priority = 5
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
      Utils.allKingCells(n, m, coord)
    }
  }

}