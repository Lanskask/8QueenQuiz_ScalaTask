package example
// sbt "runMain example.QuizFuncStyle"

import example.{Coordinate => C}

import scala.collection.mutable.ArrayBuffer

object QuizFuncStyle extends App {
//  val (n, m) = (args(0).toInt, args(1).toInt)
//  val (n, m) = (3, 3)

  var (n, m) = (4, 4) // for Rook, Bishop, Knight
  var figs = List[Figure]()
  var allCells = ArrayBuffer[C]()

  def posCount(_m: Int, _n: Int, figures: List[Figure]): Int = {
    n = _n; m = _m
    allCells = Utils.allCells(n, m)

    if(allTheSameClass(figures)) {
      return answerForSame(n, m, figs)
    } else {
      figs = figures.sortBy(x => x.priority)
      return answerForDiff(n, m, figs)
    }
  }

  var figs1 = List(Rook(n,m), Rook(n,m), Rook(n,m)).sortBy(x => x.priority)
  var figs2 = List(Rook(n,m), Bishop(n,m), Knight(n,m)).sortBy(x => x.priority)

//  posCount(8, 8, Utils.nQueens(8,8,8)) // shouldEquals 92
//  posCount(6,9,List(Kings, Kings, Queen, Rook, Knight, Bishop)) // shouldEquals 20136752
//  posCount(4, 4, figs1)
  println(posCount(4, 4, figs2) )


//  var allCells = Utils.allCells(n, m)

  def answerForSame(n: Int, m: Int, figs: List[Figure]): Int = { // for rooks
    val combinations = allCells.toList
      .combinations(figs.size)
      .toList.filter(x => scanList(x))

    combinations.foreach(println(_))
    combinations.size
  }

  def answerForDiff(n: Int, m: Int, figs: List[Figure]): Int = {
    var allPermutations = List[List[C]]()

    allCells.toList.combinations(figs.size).foreach(x => allPermutations = allPermutations ++ x.permutations.toList)

    val result = allPermutations.filter(x => scanList2(x))

    result.foreach(println(_))
    result.size
  }

  def scanList2(list: List[C]): Boolean = {
//    list.forall(x => isSafe2(x, list.indexOf(x) , list diff List(x)))
    list.forall(x => isSafe3(x, list.indexOf(x) , list diff List(x)))
  }

  /**
    * Атакует ли С другие позиции
    * @param c
    * @param figInd
    * @param others
    * @return
    */
  def isSafe2(c: C, figInd: Int, others: List[C]): Boolean = {
    if(figs(figInd).isInstanceOf[Queen])
      return others forall(!isAttackedForQueen(c, _))
    if(figs(figInd).isInstanceOf[Rook])
      return others forall(!isAttackedForRook(c, _))
    if(figs(figInd).isInstanceOf[Bishop])
      return others forall(!isAttackedForBishop(c, _))
    if(figs(figInd).isInstanceOf[Knight])
      return others forall(!isAttackedForKnight(c, _))
    if(figs(figInd).isInstanceOf[King])
      return others forall(!isAttackedForKing(c, _))
    else
      false
  }

  def isSafe3(c: C, figInd: Int, others: List[C]): Boolean = figs(figInd) match {
    case a: Queen =>
      others forall(!isAttackedForQueen(c, _))
    case a: Rook =>
        others forall(!isAttackedForRook(c, _))
    case a: Bishop =>
        others forall(!isAttackedForBishop(c, _))
    case a: Knight =>
        others forall(!isAttackedForKnight(c, _))
    case a: King =>
        others forall(!isAttackedForKing(c, _))
    case _ =>
        false
  }

    // ---------- For the same if Rooks ---
  def scanList(list: List[C]): Boolean = {
//    list.forall(x => isSafe(x, list diff List(x)))
    list.forall(x => isSafe3(x, 0, list diff List(x)))
  }

  def isSafe(c: C,/*fig: Figure, */others: List[C]): Boolean = {
//    if(fig.isInstanceOf[Rook])
    others forall(!isAttackedForRook(c, _))
  }

  // -- some little utils ---
  def allTheSameClass(figs: List[Figure]): Boolean = {
    var ans = false
    for(i <- 0 until figs.size - 1) {
      if(figs(i).getClass == figs(i+1).getClass ) ans = true
    }
    ans
  }

  // --------- is Attacked
  def isAttackedForQueen(q1: C, q2: C): Boolean = {
    q1.x == q2.x ||
      q1.y == q2.y ||
      (q2.x-q1.x).abs == (q2.y-q1.y).abs
  }

  def isAttackedForRook(r1: C, r2: C): Boolean = {
    r1.x == r2.x || r1.y == r2.y
  }

  def isAttackedForBishop(q1: C, q2: C): Boolean = { // should be
    (q2.x-q1.x).abs == (q2.y-q1.y).abs
  }

  /**
    * @param q1 knight
    * @param q2 other figure position
    * @return
    */
  def isAttackedForKnight(q1: C, q2: C): Boolean = {
      Utils.allKnightCells(n,m, q1) contains q2
  }

  def isAttackedForKing(q1: C, q2: C): Boolean = {
      Utils.allKingCells(n,m, q1) contains q2
  }

  // ---------------------
  def atackForRook(rook: C, otherRooks: Array[C]): Boolean = {
    (Utils.allLeftRightUpDown(n, m, rook) intersect otherRooks).nonEmpty
  }



}
