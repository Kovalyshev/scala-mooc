package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  def main(args: Array[String]): Unit = {
    val from = List(1, 1, 1, 2, 2, 1)
    println(s"from [$from] to [${nextLine(from)}]")
    println(s"funSeq 5 = [${funSeq.take(5).mkString("\n")}]")
  }

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    // в ФП стиле
    @tailrec
    def checkNextNum(lastNumber: Int, countNumbers: Int, lineNumbers: List[Int], acc: List[Int]): List[Int] = {
      if (lineNumbers.isEmpty) {
        (lastNumber :: countNumbers :: acc).reverse
      } else {
        val last = lineNumbers.head
        checkNextNum(
          last,
          if (last == lastNumber) countNumbers + 1 else 1,
          lineNumbers.tail,
          if (last == lastNumber) acc else lastNumber :: countNumbers :: acc
        )
      }
    }
    checkNextNum(currentLine.head, 0, currentLine, List())

    // в стиле изменяемых переменных
    /*var lastNum = currentLine.head
    var count = 0
    var out = Vector[Int]()
    for {
      num <- currentLine
    } {
      if (num == lastNum) {
        count += 1
      } else {
        out = out ++ List(count, lastNum)
        lastNum = num
        count = 1
      }
    }
    out = out ++ List(count, lastNum)
    out.toList*/
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = {
    def inf(current: List[Int]): LazyList[List[Int]] = {
      LazyList.cons(current, inf(nextLine(current)))
    }
    inf(List(1))
  }
}