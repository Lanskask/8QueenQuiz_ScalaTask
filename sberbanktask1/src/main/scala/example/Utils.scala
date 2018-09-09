package example

import example.{Coordinate => C}

import scala.collection.mutable

//class Utils(_n: Int, _m: Int) { // TODO: make  it object and call like static functions
object Utils { // TODO: make  it object and call like static functions
//  var (n, m) = (_n, _m)
  def allCells(n: Int, m: Int): mutable.ArrayBuffer[C] = {
    var allCells: mutable.ArrayBuffer[C] = mutable.ArrayBuffer()

    for(i <- 0 until n) {
      for (j <- 0 until m) {
        allCells += C(i, j)
      }
    }

    allCells
  }

  // Possible Figure movements
  def allDiagsFrom(n: Int, m: Int, inC: C): mutable.ArrayBuffer[C] = {
    var cellsOfDiags: mutable.ArrayBuffer[C] = mutable.ArrayBuffer()

    for(i <- 0 until n) {
      cellsOfDiags += C(i, i)
      cellsOfDiags += C(i, n - i - 1)
    }

    cellsOfDiags - inC - inC
  }

  // Unit tested
  def allLeftRightUpDown(n: Int, m: Int, inC: C): mutable.ArrayBuffer[C] = {
    var cellsLRUD: mutable.ArrayBuffer[C] = mutable.ArrayBuffer()

    for(i <- 0 until n)
      cellsLRUD += C(i, inC.y)
    for(j <- 0 until m)
      cellsLRUD += C(inC.x, j)

//    inBoard(cellsLRUD.distinct - inC)
    cellsLRUD - inC -inC
  }

  def allKingCells(n: Int, m: Int, inC: C): mutable.ArrayBuffer[C] = {
    var cellsKing: mutable.ArrayBuffer[C] = mutable.ArrayBuffer()

    //    if(inC.x != n - 1)
    cellsKing += C(inC.x + 1, inC.y)

    //    if(inC.x != 0)
    cellsKing += C(inC.x - 1, inC.y)

    //    if(inC.y != m - 1)
    cellsKing += C(inC.x, inC.y + 1)

    //    if(inC.y != 0)
    cellsKing += C(inC.x, inC.y - 1)

    inBoard(cellsKing, n, m)
  }

  def allKnightCells(n: Int, m: Int, inC: C): mutable.ArrayBuffer[C] = {
    var cellsKnight: mutable.ArrayBuffer[C] = mutable.ArrayBuffer()

    // 1
    //    if(inC.x != n - 1 && inC.y != m - 2)
    cellsKnight += C(inC.x + 1, inC.y + 2)

    // 2
    //    if(inC.x != n - 2 && inC.y != m - 1)
    cellsKnight += C(inC.x + 2, inC.y + 1)

    // 3
    //    if(inC.x != n - 2 && inC.y != 0)
    cellsKnight += C(inC.x + 2, inC.y - 1)

    // 4
    //    if(inC.x != n - 1 && inC.y >= 1)
    cellsKnight += C(inC.x + 1, inC.y - 2)

    // --
    // 5
    //    if(inC.x != 0 && inC.y >= 1)
    cellsKnight += C(inC.x - 1, inC.y - 2)

    // 6
    //    if(inC.x != 1 && inC.y != 0)
    cellsKnight += C(inC.x - 2, inC.y - 1)

    //     7
    //    if(inC.x != 1 && inC.y != m - 1)
    cellsKnight += C(inC.x - 2, inC.y + 1)

    // 8
    //    if(inC.x != 0 && inC.y != m - 2)
    cellsKnight += C(inC.x - 1, inC.y + 2)

    inBoard(cellsKnight, n, m)
  }

  def inBoard(arr: mutable.ArrayBuffer[C], n: Int, m: Int): mutable.ArrayBuffer[C] = {
    arr.filter{case C(x,y) => x >= 0 && x < n && y >= 0 && y < m}
  }

}
