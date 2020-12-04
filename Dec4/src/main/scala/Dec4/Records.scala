package sam.Advent2020.Dec4

import scala.io.Source

object Records {

  def clean(data: List[String]): List[String] = {
    data match {
      case List(n) => List(n)
      case n :: ns => {
        (n, clean(ns)) match {
          case ("", List(s))          => List("", s)
          case (h: String, List(s))   => List(h ++ " " + s)
          case ("", "" :: los)        => List("") ++ los
          case ("", s :: los)         => List("") ++ List(s) ++ los
          case (h: String, "" :: los) => List(h, "") ++ los
          case (h: String, s :: los)  => List(h ++ " " ++ s) ++ los
        }
      }
    }
  }

  def parse(data: List[String]): List[Map[String, String]] = {
    data.filter(i => i != "").map { entry =>
        entry.split(" ").map { kv =>
            Map(kv.split(":")(0) -> kv.split(":")(1))
          }.reduce((m1, m2) => m1 ++ m2)
    }
  }

  def loose_predicate(data: Map[String, String]): Boolean = {
    (Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").subsetOf(data.keySet))
  }

  def strict_predicate(data: Map[String, String]): Boolean = {
    val pattern1 = """^(\d{3})cm$""".r
    val pattern2 = """^(\d{2})in$""".r

    (((data("byr").toInt >= 1920) & (data("byr").toInt <= 2002))
    & ((data("iyr").toInt >= 2010) & (data("iyr").toInt <= 2020))
    & ((data("eyr").toInt >= 2020) & (data("eyr").toInt <= 2030))
    & (data("hgt") match {
      case pattern1(x) => (x.toInt >= 150 & x.toInt <= 193)
      case pattern2(x) => (x.toInt >= 59 & x.toInt <= 76)
      case _           => false
    })
    & (data("hcl").matches("""^\#[a-f0-9]{6}$"""))
    & (data("ecl").matches("""amb|blu|brn|gry|grn|hzl|oth"""))
    & (data("pid").matches("""^\d{9}$""")))
  }

  def predicate(data: List[Map[String, String]]): List[Int] = {
    data.map { m =>
      if (loose_predicate(m)) {
        if (strict_predicate(m)) 1  else 0
      } else 0
    }
  }

  def handle(data: List[String]): Int = {
    predicate(parse(clean(data))).sum
  }

  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))

  }
}

