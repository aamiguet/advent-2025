package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day10Spec extends AnyFlatSpec with should.Matchers:

  val d = Day10()

  val input =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin

  "Day 10" should "the sum of the minimum button presses to configure the indicator lights" in
    d.part1(input).shouldBe(7)

  "Day 10" should "the sum of the minimum button presses to configurer the joltage level" in
    d.part2(input).shouldBe(33)
