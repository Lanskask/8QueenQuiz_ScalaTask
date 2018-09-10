package example
// sbt "runMain example.QuizFuncStyle"

import example.{Coordinate => C}

import scala.collection.mutable.ArrayBuffer

object QuizFuncStyle extends App {
//  val (n, m) = (args(0).toInt, args(1).toInt)
  val (n, m) = (3, 3)
  val figs = List(Rook(n,m), Rook(n,m), Rook(n,m))
  var allCells = Utils.allCells(n, m)

//  /*.filter(
  //      comb => isPossible(comb.toArray)
  //    )*/
  ////    .filter(coords  => coords forall )
  ////    .filter{ coords =>
  ////      coords

  allCells.foreach(x => print(s"$x, "))

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


  def isPossible(combs: Array[C]): Boolean = {
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
  }
  //    val fig1 = figs(0)
  //    fig1.coord = comb(0)

  Utils.allCells(n,m).toList
    .combinations(figs.size)
    .toList.filter(x => scanList(x)) // .size

  // TODO: Переделать листы от C в листы от Figure

  def scanList(list: List[C]): Boolean = {
    list.forall(x => isSafe(x, list diff List(x)))
  }

  def isSafe(fig: C, others: List[C]): Boolean = {
    others forall(!isAttackedForRook(fig, _))
  }

  def isAttackedForRook(r1: C, r2: C): Boolean = {
    r1.x == r2.x || r1.y == r2.y
  }

  // ---------------------
  def atackForRook(rook: C, otherRooks: Array[C]): Boolean = {
    (Utils.allLeftRightUpDown(n, m, rook) intersect otherRooks).nonEmpty
  }



}
