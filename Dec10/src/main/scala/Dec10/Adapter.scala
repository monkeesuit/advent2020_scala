package sam.Advent2020.Dec10

import scala.io.Source
import scala.collection.mutable.Map

object Adapter {


  def handle(data: List[String]): (Int, Long) = {

    val cleanData = List(0.toLong) ++ data.map{_.toLong}.sorted
    var cache = scala.collection.mutable.Map.empty[Int, Long]


    def walk(pointer: Int, accum: List[Int]): List[Int] = {
      val value = cleanData(pointer)

      def step(i: Int): (Int, List[Int])  = {
        val newPointer = cleanData.indexOf((value.toLong + i.toLong))
        val newAccum = accum.patch((i-1) , List(accum(i-1) + 1), 1)
        (newPointer, newAccum)
      }

      pointer match {
        case a if (a == (cleanData.size - 1)) => accum.patch(2, List(accum(2) + 1), 1)
        case b if (cleanData.contains(value + 1)) => walk(step(1)._1, step(1)._2)
        case c if (cleanData.contains(value + 2)) => walk(step(2)._1, step(2)._2)
        case d if (cleanData.contains(value + 3)) => walk(step(3)._1, step(3)._2)
      }
    }


    def countPaths(pointer: Int): Long = {

      def validNeighnors(pointer: Int, step: Int): Boolean = {
        (pointer < cleanData.size - step) && (cleanData(pointer + step) - cleanData(pointer) <= 3)
      }

      def next(pointer: Int) : Long = {
        pointer match {
          case a if (a > cleanData.size - 1) => 0.toLong
          case b if (b == cleanData.size - 1) => 1.toLong
          case _ => (1 to 3).filter { i => validNeighnors(pointer, i) }.map { i => countPaths(pointer + i) }.sum
        }
      }

      cache.getOrElseUpdate(pointer, next(pointer))
    }

    val p1 = walk(0, List(0,0,0))
    val p2 = countPaths(0)
    (p1(0) * p1(2), p2)

  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))
  }
}

