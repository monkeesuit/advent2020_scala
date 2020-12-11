package sam.Advent2020.Dec11

import scala.io.Source

object SeatChart {

  def handle(data: List[String]): (Int,Int)  = {


    def initSeatingChart(row: Int, carry: Map[Coordinate, Boolean] ): Map[Coordinate, Boolean] = {
      row match {
        case a if (row == data.size) => carry
        case _ => {
          val updates = data(row).toList.zipWithIndex.map{ t =>
            val (char, col) = t
            if (char == 'L') Map(Coordinate(row, col) -> false)
            else Map()
          }.reduce{_++_}
          initSeatingChart(row + 1, carry ++ updates)
        }
      }
    }


    def walk(chartObj: SeatingChart, coord: Coordinate, direction: (Int,Int)): Boolean = {
      val nextPos = Coordinate((coord.r + direction._1), (coord.c + direction._2))

      if ((coord.r < 0) || (coord.r >= chartObj.height) || (coord.c < 0) || (coord.c >= chartObj.width)) {
        false
      } else {
        chartObj.chart.getOrElse(nextPos, walk(chartObj, nextPos, direction))
      }

    }


    def rule1(chartObj: SeatingChart): Map[Coordinate, Boolean] = {
      val currentChart = chartObj.chart
      def neighborCoordinates(coord: Coordinate): Seq[Coordinate] = Seq(
        (coord.r-1, coord.c-1), (coord.r-1, coord.c), (coord.r-1, coord.c+1),
        (coord.r, coord.c-1), (coord.r, coord.c+1),
        (coord.r+1, coord.c-1), (coord.r+1, coord.c), (coord.r+1, coord.c+1)
      ).map{Coordinate.tupled(_)}

      currentChart.keys.map{ coord =>
        val occupiedNeighbors = {
          neighborCoordinates(coord).map{ neighborCoord =>
            currentChart.getOrElse(neighborCoord, false)
          }.filter{e => e}.size
        }

        if ((currentChart(coord) == true) && (occupiedNeighbors >= 4)) {
          Map(coord -> false)
        } else if ((currentChart(coord) == false) && (occupiedNeighbors == 0)) {
          Map(coord->true)
        } else {
          Map(coord -> currentChart(coord))
        }

      }.reduce{_++_}
    }


    def rule2(chartObj: SeatingChart): Map[Coordinate, Boolean] = {
      val currentChart = chartObj.chart
      val neighborDirections = Seq(
        (-1,-1), (-1,0), (-1,1),
        (0,-1), (0,1),
        (1,-1), (1,0), (1,1)
      )

      currentChart.keys.map { seatPos =>
        val occupiedNeighbors = neighborDirections.map { direction =>
            walk(chartObj, seatPos, direction)
          }.filter{b => b}.size

        if ((currentChart(seatPos) == true) && (occupiedNeighbors >= 5)) {
          Map(seatPos -> false)
        } else if ((currentChart(seatPos) == false) && (occupiedNeighbors == 0)) {
          Map(seatPos -> true)
        } else {
          Map(seatPos -> currentChart(seatPos))
        }

      }.reduce{_++_}
    }


    def run(chartObj: SeatingChart, rule: SeatingChart => Map[Coordinate, Boolean]): Map[Coordinate, Boolean] =  {
      val currentChart = chartObj.chart
      val nextChart = rule(chartObj)

      if (currentChart.equals(nextChart)) {
        currentChart
      } else {
        run(chartObj.copy(chart = nextChart), rule)
      }

    }

    val chart0 = initSeatingChart(0, Map())
    val height = data.size
    val width = data(0).size

    val seatingChart0 = SeatingChart(height, width, chart0)
    val p1 = run(seatingChart0, rule1).filter{t => t._2}.size
    val p2 = run(seatingChart0, rule2).filter{t => t._2}.size

    (p1,p2)

  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))
  }
}
