package sam.Advent2020.Dec1

import org.scalatest._

class TwentyTwentyTest extends FlatSpec with Matchers {

 "TwentyTwenty with 2" should "return 514579" in {
  TwentyTwenty.handle(List(1721, 979, 366, 299, 675, 1456), 2, 2020) shouldBe 514579
 }
}
