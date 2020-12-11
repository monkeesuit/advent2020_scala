package sam.Advent2020.Dec11

import scala.io.Source

object SeatChart {

  def handle(data: List[String]): (Int,Int)  = {


    def initSeatingChart(row: Int, carry: Map[(Int,Int), Boolean] ): Map[(Int,Int), Boolean] = {
      row match {
        case a if (row == data.size) => carry
        case _ => {
          val updates = data(row).toList.zipWithIndex.map{ t =>
            val (char, col) = t
            if (char == 'L') Map((row, col) -> false)
            else Map()
          }.reduce{_++_}
          initSeatingChart(row + 1, carry ++ updates)
        }
      }
    }


    def walk(chart: Map[(Int,Int), Boolean],
             height: Int,
             width: Int,
             coord: (Int, Int),
             direction: (Int,Int)
            ): Boolean = {
      val nextPos = ((coord._1 + direction._1), (coord._2 + direction._2))
      if ((coord._1 < 0) || (coord._1 >= height) || (coord._2 < 0) || (coord._2 >= width)) false
      else chart.getOrElse(nextPos, walk(chart, height, width, nextPos, direction))
    }


    def rule1(chart: Map[(Int,Int), Boolean], height: Int, width: Int): Map[(Int,Int), Boolean] = {
      chart.keys.map{ k =>
        val (r,c) = k
        val occupiedNeighbors = {
          Seq(
            (r-1,c-1), (r-1,c), (r-1,c+1),
            (r,c-1), (r,c+1),
            (r+1,c-1), (r+1,c), (r+1,c+1)
          ).map{ coord =>
            chart.getOrElse(coord, false)
          }.filter{e => e}.size
        }
        if ((chart(k) == true) && (occupiedNeighbors >= 4)) Map(k -> false)
        else if ((chart(k) == false) && (occupiedNeighbors == 0)) Map(k->true)
        else Map(k -> chart(k))
      }.reduce{_++_}
    }


    def rule2(chart: Map[(Int,Int), Boolean], height: Int, width: Int): Map[(Int,Int), Boolean] = {
      chart.keys.map{ k =>
        val occupiedNeighbors = {
          Seq(
            (-1,-1), (-1,0), (-1,1),
            (0,-1), (0,1),
            (1,-1), (1,0), (1,1)
          ).map{ direction =>
            walk(chart, height, width, k, direction)
          }.filter{e => e}.size
        }
        if ((chart(k) == true) && (occupiedNeighbors >= 5)) Map(k -> false)
        else if ((chart(k) == false) && (occupiedNeighbors == 0)) Map(k->true)
        else Map(k -> chart(k))
      }.reduce{_++_}
    }


    def run(chart: Map[(Int,Int), Boolean],
            height: Int,
            width: Int,
            rule: (Map[(Int,Int), Boolean], Int, Int) => Map[(Int,Int), Boolean]
           ): Map[(Int,Int), Boolean] =  {
      val nextChart = rule(chart, height, width)
      if (chart.equals(nextChart)) chart
      else run(nextChart, height, width, rule)
    }

    val seatingChart0 = initSeatingChart(0, Map.empty[(Int,Int), Boolean])
    val height = data.size
    val width = data(0).size
    val p1 = run(seatingChart0, height, width, rule1).filter{t => t._2}.size
    val p2 = run(seatingChart0, height, width, rule2).filter{t => t._2}.size
    (p1,p2)

  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))
  }
}
