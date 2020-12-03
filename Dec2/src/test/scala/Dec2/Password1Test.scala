package sam.Advent2020.Dec2

import org.scalatest._

class Password1Test extends FlatSpec with Matchers {
  "Password1" should "return 2" in {
    Password1.handle(List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")) shouldBe 2
  }
}
