package example

/*
На прямоугольной шахматной доске размером M x N мы размещаем некоторый список шахматных фигур (кроме пешек).

Необходимо написать программу вычисляющую количество уникальных расстановок данного списка фигур на доске,
при которых ни одна из фигур
  не бьет никакую другую фигуру
    (перебором)

Тестовые примеры:
8 ферзей на доске 8 x 8  дают 92 расстановки
postCount(8, 8, List(Queen, Queen, Queen, Queen, Queen, Queen, Queen, Queen)) == 92

2 Короля, Ферзь, Ладья, Конь, Офицер на доске 6 на 9 дают 20136752 расстановки
2 Kings, Queen, Rook, Knight, Bishop

Соответственно нужно реализовать функцию
def posCount(m: Int, n: Int, figures: List[Figure]) = ???

Написать тесты на нее будет тривиально по примерам. Удачи.
* */

object Hello with App {

  def posCount(m: Int, n: Int, figures: List[Figure]): Int = {

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
