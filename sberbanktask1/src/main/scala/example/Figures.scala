package example

import scala.collection.mutable

case class Coordinate(x: Int, y: Int) {
  require(x >= 0)
  require(y >= 0)
}

trait Figure {
  val priority: Int
  var coord: Coordinate

  def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate]
}

class Board(_n: Int, _m: Int) {
  val (n, m) = (_n, _m)

  class Queen extends Figure {
    override val priority: Int = 1
    override var coord: Coordinate = Coordinate(0, 0)

    def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate] = {
      Utils.allDiagsFrom(n, m, coord) ++ Utils.allLeftRightUpDown(n, m, coord)
    }
  }

  class Rook extends Figure { // Башня
    override val priority: Int = 2
    override var coord: Coordinate = Coordinate(0, 0)

    def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate] = {
      Utils.allLeftRightUpDown(n, m, coord)
    }
  }

  class Bishop extends Figure { // Слон
    override val priority = 3
    override var coord: Coordinate = Coordinate(0, 0)

    def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate] = {
      Utils.allDiagsFrom(n, m, coord)
    }
  }

  class Knight extends Figure { // Конь
    override val priority = 4
    override var coord: Coordinate = Coordinate(0, 0)

    def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate] = {
      Utils.allKingsCells(n, m, coord)
    }
  }

  class King extends Figure {
    override val priority = 5
    override var coord: Coordinate = Coordinate(0, 0)

    def cellsReachedByFigure(coord: Coordinate): mutable.ArraySeq[Coordinate] = {
      Utils.allKingsCells(n, m, coord)
    }
  }

  // Utils
  def allCells(n: Int, m: Int): mutable.ArraySeq[Coordinate] = {
    var allCells: mutable.ArraySeq[Coordinate] = mutable.ArraySeq()

    for(i <- 0 until n) {
      for (j <- 0 until m) {
        allCells ++= mutable.ArraySeq(Coordinate(i, j))
      }
    }

    allCells
  }

  // Possible Figure movements
  def allDiagsFrom(inC: Coordinate): mutable.ArraySeq[Coordinate] = {
    var cellsOfDiags: mutable.ArraySeq[Coordinate] = mutable.ArraySeq()

    for(i <- inC.x until n) {
      for (j <- inC.y until m) {
        cellsOfDiags ++= mutable.ArraySeq(Coordinate(i, j))
      }
    }

    for(i <- inC.x to 0) {
      for (j <- inC.y to 0) {
        cellsOfDiags ++= mutable.ArraySeq(Coordinate(i, j))
      }
    }

    for(i <- inC.x until n) {
      for (j <- inC.y to 0) {
        cellsOfDiags ++= mutable.ArraySeq(Coordinate(i, j))
      }
    }

    for(i <- inC.x to 0) {
      for (j <- inC.y until m) {
        cellsOfDiags ++= mutable.ArraySeq(Coordinate(i, j))
      }
    }

    cellsOfDiags
  }

  def allLeftRightUpDown(inC: Coordinate): mutable.ArraySeq[Coordinate] = {
    var cellsLRUD: mutable.ArraySeq[Coordinate] = mutable.ArraySeq()

    for(i <- inC.x until n)
      cellsLRUD ++= mutable.ArraySeq(Coordinate(i, inC.y))

    for(i <- inC.x to 0)
      cellsLRUD ++= mutable.ArraySeq(Coordinate(i, inC.y))

    for(j <- inC.y until m)
      cellsLRUD ++= mutable.ArraySeq(Coordinate(inC.x, j))

    for(j <- inC.y to 0)
      cellsLRUD ++= mutable.ArraySeq(Coordinate(inC.x, j))

    cellsLRUD
  }

//  def allKingsCells(n: Int, m: Int, inC: Coordinate): mutable.ArraySeq[Coordinate] = {
  def allKingsCells(inC: Coordinate): mutable.ArraySeq[Coordinate] = {
    var cellsKing: mutable.ArraySeq[Coordinate] = mutable.ArraySeq()

    if(inC.x != n - 1)
      cellsKing ++= mutable.ArraySeq(Coordinate(inC.x + 1, inC.y))

    if(inC.x != 0)
      cellsKing ++= mutable.ArraySeq(Coordinate(inC.x - 1, inC.y))

    if(inC.y != m - 1)
      cellsKing ++= mutable.ArraySeq(Coordinate(inC.x, inC.y + 1))

    if(inC.y != 0)
      cellsKing ++= mutable.ArraySeq(Coordinate(inC.x, inC.y - 1))

    cellsKing
  }

  def allKnightCells(inC: Coordinate): mutable.ArraySeq[Coordinate] = {
    var cellsKnight: mutable.ArraySeq[Coordinate] = mutable.ArraySeq()

    // 1
    if(inC.x != n - 1 && inC.y != m - 2)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x + 1, inC.y + 2))

    // 2
    if(inC.x != n - 2 && inC.y != m - 1)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x + 2, inC.y + 1))

    // 3
    if(inC.x != n - 2 && inC.y != 0)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x + 2, inC.y - 1))

    // 4
    if(inC.x != n - 1 && inC.y <= 1)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x + 1, inC.y - 2))

    // 5
    if(inC.x != 0 && inC.y <= 1)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x - 1, inC.y - 2))

    // 6
    if(inC.x != 1 && inC.y != 0)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x - 2, inC.y - 1))

    // 7
    if(inC.x != 1 && inC.y != m - 1)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x - 2, inC.y + 1))

    // 8
    if(inC.x != 0 && inC.y != m - 2)
      cellsKnight ++= mutable.ArraySeq(Coordinate(inC.x - 1, inC.y + 2))

    cellsKnight
  }
}