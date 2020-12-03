package sam.Advent2020.Dec3

import scala.io.Source

object Tobogan {

  def predicate(s: String, x: Int): List[Int] = {
    if( s.toList.zipWithIndex.filter{ case (c, _) => c == '#' }.map(_._2).contains((x % s.length)) ) List(1) else List(0)
  }

  def helper(data: List[String], x: Int, xp: Int, yp: Int): List[Int] = {
    data match {
      case n if (n.size <= yp) => { predicate(n.head.trim, x) }
      case n::ns               => { predicate(n.trim, x) ++ helper(ns.drop(yp-1), x+xp, xp, yp)}
    }
  }

  def handle(data: List[String], x: Int, xp: Int, yp: Int): Int = {
    helper(data, x, xp, yp).sum
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data, 0, 3, 1))

    println((
      BigDecimal(handle(data, 0, 1, 1))
        * BigDecimal(handle(data, 0, 3, 1))
        * BigDecimal(handle(data, 0, 5, 1))
        * BigDecimal(handle(data, 0, 7, 1))
        * BigDecimal(handle(data, 0, 1, 2))
    ))
  }
}