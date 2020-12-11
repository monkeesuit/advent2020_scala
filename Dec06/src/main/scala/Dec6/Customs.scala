package sam.Advent2020.Dec6

import scala.io.Source

object Customs {

  def clean(data: Option[List[String]], accum: Option[List[(Int, String)]]): (Option[List[String]], Option[List[(Int, String)]]) = {
    (data, accum) match {
      case (_, None)                              => clean(data, Some( List( (0, "") )))
      case (Some(List()), Some(a))                => (None, Some(a))
      case (Some(n), Some(a)) if n.head == ""     => clean(Some(n.tail), Some(a ++ List((0, ""))))
      case (Some(n), Some(a)) if a.last._2 == ""  => clean(Some(n.tail), Some(a ++ List((1, n.head))))
      case (Some(n), Some(a))                     => clean(Some(n.tail), Some(a.init ++ List((a.last._1 + 1, a.last._2 + n.head))))
    }
  }

  def handle(data: List[String]): (Int, Int) = {

    val cleanData = clean(Some(data), None)._2.get.filter{ _ != (0, "") }
    val p1 = cleanData.map{ form =>
      form._2.toList.distinct.length
    }.sum
    val p2 = cleanData.map{ form =>
      val numChars = form._2.toList.groupBy(identity).mapValues(_.size)
      numChars.filter{_._2 == form._1}.size
    }.sum

    (p1,p2)
  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))

  }
}

