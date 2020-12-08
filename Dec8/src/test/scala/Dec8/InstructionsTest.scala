package sam.Advent2020.Dec8

import org.scalatest._

class InstructionsTest extends FlatSpec with Matchers {
  "Instructions" should "return 5" in {
    val test = List(
      "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    )
    Instructions.handle(test) shouldBe 5
  }
}
