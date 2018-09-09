package example

import scala.collection.mutable

import example.{Coordinate => C}

case class Coordinate(x: Int, y: Int) {
//  require(x >= 0)
//  require(y >= 0)
}

sealed trait Figure {
  val sign: String
  val priority: Int
  var coord: C

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C]
}

case class Queen(n: Int, m: Int) extends Figure {
  val sign = "Q"
  override val priority: Int = 1
  override var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allDiagsFrom(n, m, coord) ++ Utils.allLeftRightUpDown(n, m, coord)
  }
}

case class Rook(n: Int, m: Int) extends Figure {
  val sign = "R"
  val priority: Int = 2
  var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allLeftRightUpDown(n, m, coord)
  }
}

case class Bishop(n: Int, m: Int) extends Figure { // Слон
  val sign = "B"
  override val priority = 3
  override var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allDiagsFrom(n, m, coord)
  }
}

case class Knight(n: Int, m: Int) extends Figure { // Конь
  val sign = "Kn"
  override val priority = 4
  override var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allKingCells(n, m, coord)
  }
}

case class King(n: Int, m: Int) extends Figure {
  val sign = "Ki"
  override val priority = 5
  override var coord: C = C(0, 0)

  def cellsReachedByFigure(coord: C): mutable.ArrayBuffer[C] = {
    Utils.allKingCells(n, m, coord)
  }
}

class Board(_n: Int, _m: Int) {
  val (n, m) = (_n, _m)
}