package sam.Advent2020.Dec2

import org.scalatest._

class Password2Test extends FlatSpec with Matchers {
  "Password2" should "return 2" in {
    Password2.handle(List("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")) shouldBe 1
  }
}