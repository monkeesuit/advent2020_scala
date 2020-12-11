package sam.Advent2020.Dec7

import scala.io.Source

object Backpack {

  def descendentsHaveColor(find_color: String, root: String, rules: Map[String, List[Option[(Int, String)]]]): Boolean = {
    def childHaveColor(parent: String, childHasColor: Boolean): (String, Boolean) = {
      rules(parent).map{ tuple =>
        tuple match {
          case None => ("", false)
          case Some(t) => if(t._2 == find_color) ("", true) else childHaveColor(t._2, false)
        }
      }.reduce{(x,y) => ( "", x._2 || y._2)}
    }
    childHaveColor(root, false)._2
  }

  def descendentsCount(root: String, rules: Map[String, List[Option[(Int, String)]]]): Int = {

    def childCount(parent: String): Int = {
      rules(parent).map { tuple =>
        tuple match {
          case None => 0
          case Some(t) => t._1 + (t._1 * childCount(t._2))
        }
      }.sum
    }
    childCount(root)
  }

  def makeRules(data: List[String]): Map[String, List[Option[(Int, String)]]] = {
    data.map{ rules =>

      val key_string = """^(.+) bags$""".r
      val rule_string = """^(\d+) (.+) bag[s]*$""".r
      val k = rules.split("contain")(0).trim match {
        case key_string(key) => key
      }
      val v = rules.split("contain")(1).dropRight(1).split(", ").toList.map{ rule =>
        rule.trim match {
          case n if n.matches("no other bags") => None
          case rule_string(value, color) => Some(value.toInt, color)
        }
      }
      Map(k->v)
    }.reduce( (m1,m2) => m1 ++ m2)
  }


  //def handle(data: List[String]): (Int, Int) = {
  def handle(data: List[String]) = {
    val rules = makeRules(data)
    val p1 = rules.transform { (color, rule) =>
      descendentsHaveColor("shiny gold", color, rules)
    }.filter(t => t._2 == true).size

    val p2 = descendentsCount("shiny gold", rules)

    (p1, p2)
  }


  def main(args: Array[String]): Unit = {
    val f = Source.fromFile(args(0))
    val data = f.getLines.toList
    f.close

    println(handle(data))

  }
}

