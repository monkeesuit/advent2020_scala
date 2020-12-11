package sam.Advent2020.Dec4

import scala.io.Source

object Records {

  def clean(data: Option[List[String]], accum: Option[List[String]]): (Option[List[String]], Option[List[String]]) = {
    (data, accum) match {
      case (_, None)                           => clean(data, Some(List("")))
      case (Some(List()), Some(a))             => (None, Some(a))
      case (Some(n), Some(a)) if n.head == ""  => clean(Some(n.tail), Some(a ++ List("")))
      case (Some(n), Some(a)) if a.last == ""  => clean(Some(n.tail), Some(a ++ List(n.head)))
      case (Some(n), Some(a))                  => clean(Some(n.tail), Some(a.init ++ List(a.last + " " + n.head)))
    }
  }


  def parse(data: List[String]): List[Map[String, String]] = {
    data.filter(_ != "").map { entry =>
        entry.split(" ").map { kv =>
            Map(kv.split(":")(0) -> kv.split(":")(1))
          }.reduce(_++_)
    }
  }

  def loose_predicate(data: Map[String, String]): Boolean = {
    (Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").subsetOf(data.keySet))
  }

  def strict_predicate(data: Map[String, String]): Boolean = {
    val cm = """^(\d{3})cm$""".r
    val in = """^(\d{2})in$""".r

    (((data("byr").toInt >= 1920 & data("byr").toInt <= 2002))
    & ((data("iyr").toInt >= 2010) & (data("iyr").toInt <= 2020))
    & ((data("eyr").toInt >= 2020) & (data("eyr").toInt <= 2030))
    & (data("hgt") match {
      case cm(x) => (x.toInt >= 150 & x.toInt <= 193)
      case in(x) => (x.toInt >= 59 & x.toInt <= 76)
      case _           => false
    })
    & (data("hcl").matches("""^\#[a-f0-9]{6}$"""))
    & (data("ecl").matches("""amb|blu|brn|gry|grn|hzl|oth"""))
    & (data("pid").matches("""^\d{9}$""")))
  }

  def predicate(data: List[Map[String, String]]): List[Int] = {
    data.map { m =>
      if (loose_predicate(m)) { if (strict_predicate(m)) 1  else 0 }
      else 0
    }
  }

  def handle(data: List[String]): Int = {
    predicate(parse(clean(Some(data), None)._2.get)).sum
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))

  }
}

