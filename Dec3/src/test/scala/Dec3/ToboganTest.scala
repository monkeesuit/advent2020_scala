package sam.Advent2020.Dec3

import org.scalatest._

class ToboganTest extends FlatSpec with Matchers {
  "Tobogan" should "return 7" in {
    val map = List(
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    )
    Tobogan.handle(map, 3, 1) shouldBe 7
  }
}
