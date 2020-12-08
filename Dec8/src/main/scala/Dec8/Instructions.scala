package sam.Advent2020.Dec8

import scala.io.Source

object Instructions {

  val nop_string = """^(nop) ([-|+])(\d*)$""".r
  val jmp_string = """^(jmp) ([-|+])(\d*)$""".r
  val acc_string = """^(acc) ([-|+])(\d*)$""".r


  def flip_op(data: List[String], inst_num: Int): List[String] = {
    val inst_correct = data(inst_num) match {
      case nop_string(cmd, sign, num) => List(s"jmp $sign$num")
      case jmp_string(cmd, sign, num) => List(s"nop $sign$num")
    }
    data.take(inst_num) ++ inst_correct ++ data.drop(inst_num + 1)
  }


  def find_ops(data: List[String]) = {
    data.zipWithIndex.filter{ instruction =>
      instruction._1.matches("jmp [-|+].*") | instruction._1.matches("nop [-|+].*")
    }.map{_._2}
  }


  def handle(data: List[String]): (Int, Int) = {


    def execute(in_num: Int, accum: Int): (Int, Int) = {
      data(in_num) match {
        case nop_string(cmd, sign, num) =>
          (in_num.toInt + 1, accum.toInt)
        case jmp_string(cmd, sign, num) =>
          if (sign == "+") (in_num.toInt + num.toInt, accum.toInt)
          else (in_num.toInt - num.toInt, accum.toInt)
        case acc_string(cmd, sign, num) =>
          if (sign == "+") (in_num.toInt + 1, accum.toInt + num.toInt)
          else (in_num.toInt + 1, accum.toInt - num.toInt)
      }
    }


    def run(inst_num: Int, accum: Int, seen: List[Int]): (Int, Int) = {
      val (inst_next, accum_next) = execute(inst_num, accum)

      if (seen.contains(inst_next)) (inst_num, accum_next)
      else if (inst_next >= data.size) (-1, accum_next)
      else run(inst_next, accum_next, seen ++ List(inst_num))
    }

    run(0,0, List())

  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    val p1 = handle(data)
    val p2 = find_ops(data).map{ i =>
      handle(flip_op(data, i))
    }.filter{_._1 == -1}(0)

    println(p1)
    println(p2)
  }
}

