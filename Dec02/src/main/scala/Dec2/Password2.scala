package sam.Advent2020.Dec2

import scala.io.Source

object Password2 {

  def isValid(password: String, index: Int, letter: Char): Boolean = {
    try {
      (password(index - 1) == letter)
    } catch {
      case e: StringIndexOutOfBoundsException => false
    }
  }

  def handle(data: List[String]): Int = {
    val regex = """^(\d+)-(\d+)\s([a-zA-Z]):\s([a-zA-Z]*)$""".r
    data.map { i =>
      i match {
        case regex(i1, i2, l, p) =>
          if (isValid(p, i1.toInt, l(0)) ^ isValid(p, i2.toInt, l(0))) 1 else 0
        case _ => 0
      }
    }.sum
  }

  def main(args: Array[String]): Unit = {

    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))
  }
}
