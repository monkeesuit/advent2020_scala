package sam.Advent2020.Dec5

import scala.io.Source

object BoardingPass{

  def getSeatNum(seatCode: String): (Int, Int, Int) = {
    val binString = seatCode.toList.map{ ch =>
      ch match {
        case high if high.toString.matches("B|R") => "1"
        case low if low.toString.matches("F|L")   => "0"
      }
    }.mkString
    val seatNum = Integer.parseInt(sb, 2)
    val rowNum = seatNum >> 3
    val colNum = seatNum & 7
    (seatNum, rowNum, colNum)
  }

  def handle(data: List[String]): Int = {
    val seatNums = data.map{getSeatNum(_)._1}
    val taken = (0 to (seatNums.min-1)) ++ seatNums ++ ((seatNums.max+1) to 1023)
    val missing = taken.reduce(_^_)
    missing
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))

  }
}

