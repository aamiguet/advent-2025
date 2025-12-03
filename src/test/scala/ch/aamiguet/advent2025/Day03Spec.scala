package ch.aamiguet.advent2025

package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day03Spec extends AnyFlatSpec with should.Matchers:

  val d = Day03()

  val input =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111""".stripMargin

  "Day 3" should "add up the best combinations of batteries" in
    d.part1(input).shouldBe("357")

  "Day 3" should "add up the best combinations of 12 batteries" in
    d.part2(input).shouldBe("3121910778619")
