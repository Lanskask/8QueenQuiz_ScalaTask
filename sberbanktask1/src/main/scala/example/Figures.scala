package example

import scala.collection.mutable

import example.{Coordinate => C}

case class Coordinate(x: Int, y: Int) {
  require(x >= 0)
  require(y >= 0)
}

trait Figure {
  val priority: Int
  var coord: C

  def cellsReachedByFigure(coord: C): mutable.ArraySeq[C]
}

class Board(_n: Int, _m: Int) {
  val (n, m) = (_n, _m)

  case class Queen(priority: Int = 1, coord: C = C(0, 0)) extends Figure {
//    override val priority: Int = 1
//    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArraySeq[C] = {
      allDiagsFrom(coord) ++ allLeftRightUpDown(coord)
    }
  }

  class Rook extends Figure { // Башня
    override val priority: Int = 2
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArraySeq[C] = {
      allLeftRightUpDown(coord)
    }
  }

  class Bishop extends Figure { // Слон
    override val priority = 3
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArraySeq[C] = {
      allDiagsFrom(coord)
    }
  }

  class Knight extends Figure { // Конь
    override val priority = 4
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArraySeq[C] = {
      allKingsCells(coord)
    }
  }

  class King extends Figure {
    override val priority = 5
    override var coord: C = C(0, 0)

    def cellsReachedByFigure(coord: C): mutable.ArraySeq[C] = {
      allKingsCells(coord)
    }
  }

  // Utils
  def allCells: mutable.ArraySeq[C] = {
    var allCells: mutable.ArraySeq[C] = mutable.ArraySeq()

    for(i <- 0 until n) {
      for (j <- 0 until m) {
        allCells ++= mutable.ArraySeq(C(i, j))
      }
    }

    allCells
  }

  // Possible Figure movements
  def allDiagsFrom(inC: C): mutable.ArraySeq[C] = {
    var cellsOfDiags: mutable.ArraySeq[C] = mutable.ArraySeq()

    for(i <- inC.x until n) {
      for (j <- inC.y until m) {
        cellsOfDiags ++= mutable.ArraySeq(C(i, j))
      }
    }

    for(i <- inC.x to 0) {
      for (j <- inC.y to 0) {
        cellsOfDiags ++= mutable.ArraySeq(C(i, j))
      }
    }

    for(i <- inC.x until n) {
      for (j <- inC.y to 0) {
        cellsOfDiags ++= mutable.ArraySeq(C(i, j))
      }
    }

    for(i <- inC.x to 0) {
      for (j <- inC.y until m) {
        cellsOfDiags ++= mutable.ArraySeq(C(i, j))
      }
    }

    cellsOfDiags
  }

  def allLeftRightUpDown(inC: C): mutable.ArraySeq[C] = {
    var cellsLRUD: mutable.ArraySeq[C] = mutable.ArraySeq()

    for(i <- inC.x until n)
      cellsLRUD ++= mutable.ArraySeq(C(i, inC.y))

    for(i <- inC.x to 0)
      cellsLRUD ++= mutable.ArraySeq(C(i, inC.y))

    for(j <- inC.y until m)
      cellsLRUD ++= mutable.ArraySeq(C(inC.x, j))

    for(j <- inC.y to 0)
      cellsLRUD ++= mutable.ArraySeq(C(inC.x, j))

    cellsLRUD
  }

  def allKingsCells(inC: C): mutable.ArraySeq[C] = {
    var cellsKing: mutable.ArraySeq[C] = mutable.ArraySeq()

//    if(inC.x != n - 1)
      cellsKing ++= mutable.ArraySeq(C(inC.x + 1, inC.y))

//    if(inC.x != 0)
      cellsKing ++= mutable.ArraySeq(C(inC.x - 1, inC.y))

//    if(inC.y != m - 1)
      cellsKing ++= mutable.ArraySeq(C(inC.x, inC.y + 1))

//    if(inC.y != 0)
      cellsKing ++= mutable.ArraySeq(C(inC.x, inC.y - 1))

    cellsKing.filter{case C(x,y) => x >= 0 && y >= 0}
  }

  def allKnightCells(inC: C): mutable.ArraySeq[C] = {
    var cellsKnight: mutable.ArraySeq[C] = mutable.ArraySeq()

    // 1
//    if(inC.x != n - 1 && inC.y != m - 2)
      cellsKnight ++= mutable.ArraySeq(C(inC.x + 1, inC.y + 2))

    // 2
//    if(inC.x != n - 2 && inC.y != m - 1)
      cellsKnight ++= mutable.ArraySeq(C(inC.x + 2, inC.y + 1))

    // 3
//    if(inC.x != n - 2 && inC.y != 0)
      cellsKnight ++= mutable.ArraySeq(C(inC.x + 2, inC.y - 1))

    // 4
//    if(inC.x != n - 1 && inC.y >= 1)
      cellsKnight ++= mutable.ArraySeq(C(inC.x + 1, inC.y - 2))

    // --
    // 5
//    if(inC.x != 0 && inC.y >= 1)
      cellsKnight ++= mutable.ArraySeq(C(inC.x - 1, inC.y - 2))

    // 6
//    if(inC.x != 1 && inC.y != 0)
      cellsKnight ++= mutable.ArraySeq(C(inC.x - 2, inC.y - 1))

//     7
//    if(inC.x != 1 && inC.y != m - 1)
      cellsKnight ++= mutable.ArraySeq(C(inC.x - 2, inC.y + 1))

    // 8
//    if(inC.x != 0 && inC.y != m - 2)
      cellsKnight ++= mutable.ArraySeq(C(inC.x - 1, inC.y + 2))

    cellsKnight.filter{case C(x,y) => x >= 0 && y >= 0}
  }
}