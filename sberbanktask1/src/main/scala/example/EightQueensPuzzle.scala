package example

import example.{Coordinate => C}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  var allCells = ArrayBuffer[C]()
  var presPossits = ArrayBuffer[C]()
  var ANSWER: Int = 0

  var n: Int = _; var m: Int = _
  var figs = ArrayBuffer[Figure]()
  val numOfFields = n * m // boardSize
  var figInd: Int = 0
  var firstFigPosInd: Int = 0
  var resultCollocations = ArrayBuffer[Figure]()

  def posCount(_m: Int, _n: Int, figures: List[Figure]): Int = {
    // Инициализация
    n = _n; m = _m
    figures.foreach(fig => figs += fig)
    figs = figs.sortBy(x => x.priority)
    allCells = Utils.allCells(n, m)

    basicFun()
  }

  def basicFun()= {
    // Ставим сначала на первое место первую фигуру
    // Ставим первую фигуру по всем клеткам доски
    for(firstFigPosInd_2 <- 0 until numOfFields) {
      servFigs(firstFigPosInd_2, 0)
    }

//    for(firstFigPosInd_2 <- 0 until numOfFields) {
//      for(figInd_2 <- figs.indices) {
//        servFigs(firstFigPosInd_2, figInd_2)
//      }
//    }
    
    servFigs(0, 0 /*с первой фигуры*/)

    ANSWER
  }

  // Если фигуры кончились, то ANSWER + 1
  //    записываем эту расстановку в память удавшихся расстановок
  //    можно поставить последнюю фигуру в другую клетку в отличии от первой
  //    (backtracking)

  def servFigs(_firstFigPosInd: Int, figInd: Int): Int = {
    if(figInd < figs.size - 1) { // for(i <- 0 until numOfFields) {
//        servThisFig(0, figs(figInd))
      minusThisFig(0, figs(figInd))
    } else {
      ANSWER += 1
    }

    ANSWER
  }

  def minusThisFig(inAllCellsPos: Int, fig: Figure): Unit = {
    // Если inAllCellsPos больше размера доски (клетка за доской),
    //  то выход из этой функции
    //  и ставим первую фигуру в новую позицию
    if (inAllCellsPos > numOfFields) {
      servFigs(firstFigPosInd + 1,  0)
    }

    fig.coord = allCells(inAllCellsPos) // ставим фигуру на эту координату

    // Проверяем пересекаются ли пути этой фигуры с позициями предыдущих фигурам
    // если пересекается-> перемещаем эту фигуру в следущую клетку
    if ((presPossits intersect fig.cellsReachedByFigure(fig.coord)).nonEmpty) {
      minusThisFig(inAllCellsPos + 1, fig)
    } else {
      // Если не пересекается,
      //  записываем эту фигуру
      //  выписываем из всех оставшихся клеток доски позицию этой фигуры
      //  выписываем из всех оставшихся клеток доски, те клетки, которые пересекает эта фигура
      // и переходим к другой фигуре
      presPossits += fig.coord
      allCells -= fig.coord
      allCells --= fig.cellsReachedByFigure(fig.coord)
      servFigs(firstFigPosInd, figInd + 1) // переходим к следующей фигуре
    }
  }

  //  def servThisFig(i: Int, j: Int/**/, fig: Figure): Unit = {
//    fig.coord = allCells(0)
//
//    // Если пересекается -> перемещаем эту фигуру в следущую клетку
//    if ((presPossits intersect fig.cellsReachedByFigure(fig.coord)).nonEmpty) {
//      servThisFig(i, j + 1 /**/ , fig)
//    } else { // Если не пересекается, записываем эту фигуру и переходим к другой фигуре
//      allCells -= fig.coord
//      presPossits += fig.coord
//      allCells --= fig.cellsReachedByFigure(fig.coord)
//      servFigs(i + 1, n, m)
//    }
//  }


  // ------------------


  def posCount2(m: Int, n: Int, figures: List[Figure]): Int = {

    /*for(i <- figs.indices) {
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

}
