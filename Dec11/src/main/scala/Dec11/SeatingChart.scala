package sam.Advent2020.Dec11

import scala.io.Source

case class Coordinate(r: Int, c: Int)

case class SeatingChart(height: Int, width: Int, chart: Map[Coordinate, Boolean])