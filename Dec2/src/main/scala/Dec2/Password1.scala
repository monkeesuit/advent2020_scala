package sam.Advent2020.Dec2

import scala.io.Source

object Password1 {

  def handle(data: List[String]): Int = {
    val regex = """^(\d+)-(\d+)\s([a-zA-Z]):\s([a-zA-Z]*)$""".r
    data.map { i =>
      i match {
        case regex(lb, ub, l, p) =>
          if (p.count(_ == l(0)) >= lb.toInt & p.count(_ == l(0)) <= ub.toInt) 1
          else 0
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
