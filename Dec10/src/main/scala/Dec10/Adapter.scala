package sam.Advent2020.Dec10

import scala.io.Source
import scala.collection.mutable.Map

object Adapter {


  def handle(data: List[String]): (Int, Long) = {

    val cleanData = List(0.toLong) ++ data.map{_.toLong}.sorted
    var memo = Map[Int, Long]()

    def tryWalk(pointer: Int, accum: Option[List[Int]]): Option[List[Int]] = {
      if (pointer == (cleanData.size - 1)) return Some(accum.get.patch(2 , List(accum.get(2) + 1), 1))
      val value = cleanData(pointer)
      val jump =  {
        if (cleanData.contains(value + 1)) Some(1)
        else if (cleanData.contains(value + 2)) Some(2)
        else if (cleanData.contains(value + 3)) Some(3)
        else None
      }
      jump match {
        case None => None
        case Some(i: Int) => {
          val new_pointer = cleanData.indexOf((value.toLong + i.toLong))
          val current_accum = accum.getOrElse(List(0,0,0))
          val new_accum = current_accum.patch((i-1) , List(current_accum(i-1) + 1), 1)
          tryWalk(new_pointer, Some(new_accum))
        }
      }
    }

    def countPaths(pointer: Int): Long = {

      memo.getOrElse(pointer, None) match{
        case None => ()
        case _ => return memo(pointer)
      }
      if (pointer > cleanData.size - 1) {
        memo(pointer) = 0.toLong
        return memo(pointer)
      }
      if (pointer == cleanData.size - 1) {
        memo(pointer) = 1.toLong
        return memo(pointer)
      }
      def validNeighnors(step: Int): Boolean = {
        (pointer < cleanData.size - step) && (cleanData(pointer + step) - cleanData(pointer) <= 3)
      }
      val oneStep = if (validNeighnors(1)) {
        countPaths(pointer + 1)
      } else {
        0.toLong
      }
      val twoStep = if (validNeighnors(2)) {
        countPaths(pointer + 2)
      } else {
        0.toLong
      }
      val threeStep = if (validNeighnors(3)) {
        countPaths(pointer + 3)
      } else {
        0.toLong
      }
      memo(pointer) = (oneStep.toLong + twoStep.toLong + threeStep.toLong)
      return memo(pointer)
    }

    val p1 = tryWalk(0, None) match {
      case None => None
      case Some(l: List[Int]) => Some(l(0) * l(2))
    }

    val p2 = countPaths(0)

    (p1.get, p2)

  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))
  }
}

