package sam.Advent2020.Dec6

import org.scalatest._

class CustomsTest extends FlatSpec with Matchers {
  "Customs" should "return 11" in {
    val test = List(
      "abc",
      "",
      "a",
      "b",
      "c",
      "",
      "ab",
      "ac",
      "",
      "a",
      "a",
      "a",
      "a",
      "",
      "b"
    )
    Customs.handle(test) shouldBe (11, 6)
  }
}
