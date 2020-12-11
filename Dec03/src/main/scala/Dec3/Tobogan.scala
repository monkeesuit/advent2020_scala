package sam.Advent2020.Dec3

import scala.io.Source

object Tobogan {

  def predicate(s: String, x: Int): List[Int] = {
    if (s.toList(x % s.length) == '#') List(1) else List(0)
  }

  def helper(data: List[String], x: Int, xp: Int, yp: Int): List[Int] = {
    data match {
      case n if (n.size <= yp) => { predicate(n.head.trim, x) }
      case n :: ns => {
        predicate(n.trim, x) ++ helper(ns.drop(yp - 1), x + xp, xp, yp)
      }
    }
  }

  def handle(data: List[String], xp: Int, yp: Int): Int = {
    helper(data, 0, xp, yp).sum
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data, 3, 1))

    println(
      (
        BigDecimal(handle(data, 1, 1))
          * BigDecimal(handle(data, 3, 1))
          * BigDecimal(handle(data, 5, 1))
          * BigDecimal(handle(data, 7, 1))
          * BigDecimal(handle(data, 1, 2))
      )
    )
  }
}
