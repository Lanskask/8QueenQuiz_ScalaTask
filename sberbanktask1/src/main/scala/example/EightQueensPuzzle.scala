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
posCount(8, 8, List(Queen, Queen, Queen, Queen, Queen, Queen, Queen, Queen)) shouldEquals 92

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
//  var numOfFields = n * m // boardSize
  var figInd: Int = 0
  var firstFigPosInd: Int = 0
  var thisCollocation = ArrayBuffer[Figure]() // массив фигур - их координаты, типы и названия
  var resultCollocations = ArrayBuffer[ArrayBuffer[Figure]]()

  run()

  def run(): Unit = {
    posCount(8, 8, Utils.nQueens(8,8,8))
  }

  def posCount(_m: Int, _n: Int, figures: List[Figure]): Int = {
    // Инициализация
    initialization(_m, _n, figures)

    // Ставим первую фигуру из отсортированного по приоритету массива фигур по всем клеткам доски
//    for(firstFigPosInd_2 <- 0 until numOfFields) {
//      servFigs(firstFigPosInd_2, 0)
//    }

    // Если все фигуры кончились
    //  если каждая фигура начиная с конца поменяла своё первое положение по всем клеткам на которым она изначально могла встать
    //  если первая фигура поменяла своё место положение по всем клеткам
    //
    // То заканциваем пробегание, заканчиваемм программу и выписываем результат
    forAllCells(0,0)
    ANSWER = thisCollocation.distinct.size

    printResult(ANSWER)

    ANSWER
//        thisCollocation.distinct.size
  }

  // TODO: Сделать backtracking по графу
  // TOD: Сделать Класс (или type) Collocation: ArrayBuffer[ArrayBuffer[Figure]()]()

  // ---- TODO: Куда это вставить ---------- ?
  // Если фигуры при построении этой расстановки (thisCollocation) кончились, то ANSWER + 1
  //    записываем эту расстановку в память удавшихся расстановок:    resultCollocations += thisCollocation
  //    можно пойти вверх по графу этой расстановки
  //      зная какие там свободные клетки, можно поставить обрабатываемую фигуру в другую клетку
  //      и провести операции по проверке пересечения ещё раз
  //    можно поставить последнюю фигуру в другую клетку в отличии от первой
  //      (backtracking)


  def forAllCells(figInd: Int, cellInd: Int/*, allCells: ArrayBuffer[Figure]()*//*, numOfFields*/): Unit = {
    // для всех свободных клеток этой выбранной фигуры
    for(cellInd <- 0 until (n * m)) {
      // для всех остальных фигур
      forAllOtherFigs(cellInd, figInd)
      resultCollocations += thisCollocation

      // ДеИнициализация
      // Опустошение предыдуще заполненных массивов
      deinitialization()
    }
  }

  // TODO: Куда вставлять в этой функции cellInd, а куда figInd ?
  def forAllOtherFigs(_figInd: Int, cellInd: Int): Unit = {

    // TODO: Что такое вообще _firstFigPosInd ? На что заменить?
    for (figInd <- _figInd to figs.size) { // TODO: Здесь _figInd или _figInd + 1 ?
      var fig = figs(figInd)
      fig.coord = allCells(cellInd)
//      if (isIntersect(fig , presPossits)) {
      if (isIntersectColoc(fig , thisCollocation)) {
        // Проверяем пересекаются ли пути этой фигуры с позициями предыдущих фигурам
        // если пересекается-> перемещаем эту фигуру в следущую клетку
        //    if ((presPossits intersect fig.cellsReachedByFigure(fig.coord)).nonEmpty) {
        forAllCells(figInd, cellInd + 1) // фигура таже клетка другая
      } else {
        // Если не пересекается,
        //  записываем позицию этой фигуры в предыдущие позиции фигур
        //  записываем эту фигуру в массив всех фигур уже расставленных
        //  выписываем из всех оставшихся клеток доски позицию этой фигуры
        //  выписываем из всех оставшихся клеток доски, те клетки, которые пересекает эта фигура
        // и переходим к другой фигуре
//        presPossits += fig.coord
        thisCollocation += fig
        allCells -= fig.coord
        allCells --= fig.cellsReachedByFigure(fig.coord)
        forAllOtherFigs(figInd + 1, cellInd) // переходим к следующей фигуре
      }
    }
  }

  // ------ Old variants -------------
  /*def servFigs(_firstFigPosInd: Int, figInd: Int): Int = {
    if(figInd < figs.size - 1) { // for(i <- 0 until numOfFields) {
      //        servThisFig(0, figs(figInd))
      minusThisFig(0, figs(figInd))
    } else {
      ANSWER += 1
    }

    ANSWER
  }*/

  /*def minusThisFig(inAllCellsPos: Int, fig: Figure): Unit = {
    // Если inAllCellsPos больше размера доски (клетка за доской),
    //  то выход из этой функции
    //  и ставим первую фигуру в новую позицию
    if (inAllCellsPos > numOfFields) {
      servFigs(firstFigPosInd + 1,  0)
    }

    fig.coord = allCells(inAllCellsPos) // ставим фигуру на эту координату

    // Проверяем пересекаются ли пути этой фигуры с позициями предыдущих фигурам
    // если пересекается-> перемещаем эту фигуру в следущую клетку
//    if ((presPossits intersect fig.cellsReachedByFigure(fig.coord)).nonEmpty) {
    if (isIntersect(fig, presPossits)) {
      minusThisFig(inAllCellsPos + 1, fig)
    } else {
      // Если не пересекается,
      //  записываем позицию этой фигуры в предыдущие позиции фигур
      //  записываем эту фигуру в массив всех фигур уже расставленных
      //  выписываем из всех оставшихся клеток доски позицию этой фигуры
      //  выписываем из всех оставшихся клеток доски, те клетки, которые пересекает эта фигура
      // и переходим к другой фигуре
      presPossits += fig.coord
      thisCollocation += fig
      allCells -= fig.coord
      allCells --= fig.cellsReachedByFigure(fig.coord)
      servFigs(firstFigPosInd, figInd + 1) // переходим к следующей фигуре
    }
  }*/

  // -------------------
  def initialization(_m: Int, _n: Int, figures: List[Figure]) = {
    n = _n; m = _m
    figures.foreach(fig => figs += fig)
    figs = figs.sortBy(x => x.priority)
    allCells = Utils.allCells(n, m)
  }

  // Опустошение предыдуще заполненных массивов
  def deinitialization() = {
    figInd = 0
    firstFigPosInd = 0
    thisCollocation = ArrayBuffer[Figure]()
    presPossits = ArrayBuffer[C]()
    allCells = Utils.allCells(n, m)
  }

  def isIntersect(fig: Figure, presPos: ArrayBuffer[C]): Boolean =
    (fig.cellsReachedByFigure(fig.coord) intersect presPossits).nonEmpty

  // TODO: Check this method
  def isIntersectColoc(fig: Figure, thisCollocation: ArrayBuffer[Figure]): Boolean = {
    var presPos = ArrayBuffer[C]()
    thisCollocation.foreach(figure => presPos += figure.coord)
    (fig.cellsReachedByFigure(fig.coord) intersect presPos).nonEmpty
  }

  def printResult(answer: Int): Unit = {
    println(s"Для размера доски n = $n, m = $m и набора фигур: ")
    figs.foreach(x => print(s"$x, "))
    println(s"\nсуществует $answer перестановок фигур, таких что ни одна из них не бьёт другую ")
  }

}
