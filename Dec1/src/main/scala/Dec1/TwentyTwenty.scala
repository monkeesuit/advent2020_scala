package sam.Advent2020.Dec1

import scala.io.Source

object TwentyTwenty {

  def handle(data: List[Int], size: Int, sum: Int): Int = {
    val ri = data.toSet.subsets.filter { p =>
      p.size == size & p.sum == sum
    }
    ri.next.toList.reduce((x, y) => x * y)
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList.map(_.toInt)
    f.close

    println(handle(data, args(1).toInt, args(2).toInt))
  }

}
