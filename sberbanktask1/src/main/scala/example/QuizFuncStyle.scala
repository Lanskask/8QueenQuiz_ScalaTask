package example
// sbt "runMain example.QuizFuncStyle"

import example.{Coordinate => C}

import scala.collection.mutable.ArrayBuffer

object QuizFuncStyle extends App {
//  val (n, m) = (args(0).toInt, args(1).toInt)
//  val (n, m) = (3, 3)
  val (n, m) = (4, 4) // for Rook, Bishop, Knight
//  var figs = List(Rook(n,m), Rook(n,m), Rook(n,m)).sortBy(x => x.priority)
  var figs = List(Rook(n,m), Bishop(n,m), Knight(n,m)).sortBy(x => x.priority)
  var allCells = Utils.allCells(n, m)

  //  /*.filter(
  //      comb => isPossible(comb.toArray)
  //    )*/
  ////    .filter(coords  => coords forall )
  ////    .filter{ coords =>
  ////      coords

//  allCells.foreach(x => print(s"$x, "))

//  allCells.combinations(figs.size) // figs.size = 3
//        .combinations(2)
//        .forall{
//          case List(c1,c2) =>
//            c1.x != c2.x 	&&
//              c1.y != c2.y
////      }
//    }
//    .foreach(println(_))
//    .filter(comb => isSafe(comb.toArray)).foreach(println(_))

  /*def isPossible(combs: Array[C]): Boolean = {
    var answ = false
    for(comb <- combs) {
//      if(isIntersectColoc(fig1, ))
//        if(atackForRook(comb, combs diff List(comb)))
//        if(isSafe(comb, combs diff List(comb)))
        if(isSafe(comb, combs))
          answ = false
        else
          answ = true
    }
    answ
  }*/
  //    val fig1 = figs(0)
  //    fig1.coord = comb(0)

  var allPermutations = List[List[C]]()

  allCells.toList.combinations(3).foreach(x => allPermutations = allPermutations ++ x.permutations.toList)

  allPermutations.foreach(println(_))

  var ans2 = allPermutations.count(x => scanListVarios(x))

  println(ans2)

  // -----------------
  /*def answerForRooks(n: Int, m: Int, figs: List[Rook]): Int = { // for rooks
    Utils.allCells(n, m).toList
      .combinations(figs.size)
      .toList.count(x => scanList(x))
  }*/

  // TODO: Переделать листы от C в листы от Figure

  def scanListVarios(list: List[C]): Boolean = {
    list.forall(x => isSafeDiff(x, list.indexOf(x) , list diff List(x)))
  }

  /**
    * Атакует ли С другие позиции
    * @param c
    * @param figInd
    * @param others
    * @return
    */
  def isSafeDiff(c: C, figInd: Int, others: List[C]): Boolean = {
    var ans = false
    if(figs(figInd).isInstanceOf[Queen])
      ans = others forall(!isAttackedForQueen(c, _))
    if(figs(figInd).isInstanceOf[Rook])
      ans = others forall(!isAttackedForRook(c, _))
    if(figs(figInd).isInstanceOf[Bishop])
      ans = others forall(!isAttackedForBishop(c, _))
    if(figs(figInd).isInstanceOf[Knight])
      ans = others forall(!isAttackedForKnight(c, _))
    if(figs(figInd).isInstanceOf[King])
      ans = others forall(!isAttackedForKing(c, _))

    ans
  }

  // ---------- For the same ---
  def scanList(list: List[C]): Boolean = {
    list.forall(x => isSafe(x, list diff List(x)))
  }

  def isSafe(c: C,/*fig: Figure, */others: List[C]): Boolean = {
//    if(fig.isInstanceOf[Rook])
    others forall(!isAttackedForRook(c, _))
  }

  // is Attacked
  def isAttackedForQueen(q1: C, q2: C): Boolean = {
    q1.x == q2.x ||
      q1.y == q2.y ||
      (q2.x-q1.x).abs == (q2.y-q1.y).abs
  }

  def isAttackedForRook(r1: C, r2: C): Boolean = {
    r1.x == r2.x || r1.y == r2.y
  }

  // TODO: Check this func
  def isAttackedForBishop(q1: C, q2: C): Boolean = { // should be
    Utils.allDiagsFrom(n,m, q1) contains q2 // or
    //    (q2.x-q1.x).abs == (q2.y-q1.y).abs
  }

  /**
    * @param q1 knight
    * @param q2 other figure position
    * @return
    */
  def isAttackedForKnight(q1: C, q2: C): Boolean = {
//    q1.x == q2.x ||
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
