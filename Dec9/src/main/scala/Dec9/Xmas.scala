package sam.Advent2020.Dec9

import scala.io.Source

object Xmas {


  def handle(data: List[String], preambleSize: Int): (Long, Long) = {

    val cleanData = data.map(_.toLong)
    
    def findSeed(pointer: Int): Option[Long] = {

      def isValid(preamble: List[Long], valueToCheck: Long): Boolean = {
        preamble.zipWithIndex.map{ e =>

          val (preambleValue, preambleIndex) = e

          val removed = if (preambleIndex == 0) {
            preamble.tail
          } else if (preambleIndex == preambleSize) {
            preamble.init
          } else {
            preamble.splitAt(preambleIndex)._1 ++ preamble.splitAt(preambleIndex)._2.tail
          }
          removed.contains(valueToCheck - preambleValue)

        }.reduce{_|_}
      }

      if (pointer >= cleanData.size - preambleSize) None

      val preamble = cleanData.slice(pointer, pointer + preambleSize)
      val valueToCheck = cleanData(pointer + preambleSize)

      isValid(preamble, valueToCheck) match {
        case false => Some(valueToCheck)
        case true => findSeed(pointer + 1)
      }
    }


    def findKey(pointer: Int, seed: Long): Long = {

      def checkSeq(index: Int, accum: List[Long]): Option[List[Long]] = {
        val value = cleanData(index)
        if (value + accum.sum < seed) checkSeq((index + 1), accum ++ List(value))
        else if ((value + accum.sum == seed) && (accum.size >= 2)) Some(accum ++ List(value))
        else None
      }

      checkSeq(pointer, List(0.toLong)) match {
        case None => findKey((pointer + 1), seed)
        case Some(v) => v.drop(1).min + v.max
      }

    }

    (findSeed(0).get, findKey(0, findSeed(0).get))

  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data, 25))
  }
}

