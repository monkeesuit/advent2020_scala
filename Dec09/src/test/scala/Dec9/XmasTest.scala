package sam.Advent2020.Dec9

import org.scalatest._

class XmasTest extends FlatSpec with Matchers {
  "Xmas" should "return (127, 62)" in {
    val test = List(
      "35",
      "20",
      "15",
      "25",
      "47",
      "40",
      "62",
      "55",
      "65",
      "95",
      "102",
      "117",
      "150",
      "182",
      "127",
      "219",
      "299",
      "277",
      "309",
      "576")
    Xmas.handle(test, 5) shouldBe (127, 62)
  }
}
