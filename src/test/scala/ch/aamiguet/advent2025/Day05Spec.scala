package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day05Spec extends AnyFlatSpec with should.Matchers:

  val d = Day05()

  val input =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32""".stripMargin

  "Day 5" should "find all the ingredients" in
    d.part1(input).shouldBe("3")

  "Day 5" should "find all the fresh ids" in
    d.part2(input).shouldBe("14")
