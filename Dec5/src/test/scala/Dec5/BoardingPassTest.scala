package sam.Advent2020.Dec5

import org.scalatest._

class BoardingPassTest extends FlatSpec with Matchers {
  "Records" should "return 2" in {
    val test = List(
      "BFFFBBFRRR", row 70, column 7, seat ID 567.
    "FFFBBBFRRR" row 14, column 7, seat ID 119.
    "BBFFBBFRLL" row 102, column 4, seat ID 820.
    Records.handle(test) shouldBe [567,]
  }
}
