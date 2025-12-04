package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day04Spec extends AnyFlatSpec with should.Matchers:
  val d = Day04()

  val input =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.""".stripMargin

  "Day 4" should "find the 13 accessible rolls" in
    d.part1(input).shouldBe("13")

  "Day 4" should "find the 43 removable rolls" in
    d.part2(input).shouldBe("43")
