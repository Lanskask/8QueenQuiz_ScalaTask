package example

import example.{Coordinate => C}

import scala.collection.mutable

/*
На прямоугольной шахматной доске размером M x N мы размещаем некоторый список шахматных фигур (кроме пешек).

Необходимо написать программу вычисляющую количество уникальных расстановок данного списка фигур на доске,
при которых ни одна из фигур
  не бьет никакую другую фигуру
    (перебором)

Тестовые примеры:
8 ферзей на доске 8 x 8  дают 92 расстановки
posCount(8, 8, List(Queen, Queen, Queen, Queen, Queen, Queen, Queen, Queen)) shouldEquals  92

2 Короля, Ферзь, Ладья, Конь, Офицер на доске 6 на 9 дают 20136752 расстановки
posCount(6,9,List(Kings, Kings, Queen, Rook, Knight, Bishop)) shouldEquals 20136752

Соответственно нужно реализовать функцию
def posCount(m: Int, n: Int, figures: List[Figure]) = ???

Написать тесты на нее будет тривиально по примерам. Удачи.

*/

// EightQueensPuzzle
object EightQueensPuzzle extends App {


  def basicFun(n: Int, m: Int, figs: mutable.ArrayBuffer[Figure])= {
    var ANSWER: Int = 0
//    var beginIndInAllCells = 0

    val numOfFields = n * m
    var allCells = Utils.allCells(n, m)

    for(i <- 0 until numOfFields) {
      for(j <- figs.indices) {

      }

    }
    //    def basicFunIn(): Unit = {

      def servFigs(i: Int, n: Int, m: Int): Unit = {
        if(i > figs.size - 1) { // for(i <- 0 until numOfFields) {
          ANSWER += 1
  //        beginIndInAllCells += 1
          allCells -= allCells(0)
          // выйти из этой функции вообще
//          basicFunIn()
        }

        var presPossits: mutable.ArrayBuffer[C] = mutable.ArrayBuffer[C]()
                          // for(j <- figs.indices) {
        def servThisFig(fig: Figure): Unit = {
          fig.coord = allCells(0)

          if (i == 0 || (presPossits intersect fig.cellsReachedByFigure(fig.coord)).nonEmpty) {
            allCells -= fig.coord
            servThisFig(fig)
          } else {
            presPossits += fig.coord
            allCells --= fig.cellsReachedByFigure(fig.coord)
            servFigs(i + 1, n, m)
          }
        }

        servThisFig(figs(i))
      }
      servFigs(0, n, m)
//    }
    ANSWER
  }

  def posCount(m: Int, n: Int, figures: List[Figure]): Int = {
    var figs = mutable.ArrayBuffer[Figure]()
    figures.foreach(fig => figs += fig)
    figs = figs.sortBy(x => x.priority)

    basicFun(n, m, figs)
  }

  def posCount2(m: Int, n: Int, figures: List[Figure]): Int = {
//    val board: Board = new Board(n,m)
//    val figs = figures.toArray


    /*
    var allCells = Utils.allCells(n, m)

    for(i <- figs.indices) {
      figs(i).coord = allCells(0); allCells -= allCells(0)
      allCells --= figs(i).cellsReachedByFigure(figs(i).coord)
      // дали следующей фигуре песрвый из оставшихся allCells; убрали эту координатуу из allCells
      figs(i+1).coord = allCells(0); allCells -= allCells(0)
      // var previosPositions = mutable.ArrayBuffer()
      //     for fig in (figs 'downwords') previosPositions += fig.coord
      if(figs(i+1).cellsReachedByFigure(figs(i+1).coord ) contains previosPositions
      )
        figs(i+1).coord = allCells(0)*/

//    .foreach{ figure =>
//      figure.coord = allCells(0)
//      allCells = allCells - allCells(0)
//      allCells --= figure.cellsReachedByFigure(figure.coord)
//    }
    5
  }

  /*
  * 1. AllBoardCells
  * 2. sort input Figures Array by Priority
  *
  * 3. place First figure to the first Possition of the AllBoardCells
  * 4. AllBoardCells -- AllPossiblePositionsOf(figure) = OtherPositions1
  * 4.1. поставить фигуру i + 1 (следующую) на точку в OtherPositions1
  * 5. AllPossiblePositionsOf(figure2)
  * // 5.1 if( !AllPosPossOf(figure2).contains(figures(n - 1).coord)) continue
  * 5.2 if( AllPosPossOf(figure2).contains(figures(n - 1).coord))
  *         поставить фигуру 2 на другую точку из allCells
  *         и повторить эту проверку (5.2) ещё раз
  *
  *
  *
  *
  * 6. if( (OtherPositions1 intersect AllPosPossOf(figure2)).size == 0 ) // no intersection
  *
  *
  * Трэк второй фигуры не пересекает координату первой
  *   Трэк фигуры n + 1  не пересекает figures(N).coord
  *
  *
  *
  *
  *
  * */
}
