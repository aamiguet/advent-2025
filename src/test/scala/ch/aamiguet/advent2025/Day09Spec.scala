package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day09Spec extends AnyFlatSpec with should.Matchers:

  val d = Day09()

  val input =
    """7,1
      |11,1
      |11,7
      |9,7
      |9,5
      |2,5
      |2,3
      |7,3""".stripMargin

  "Day 9" should "find the largest rectangle" in
    d.part1(input).shouldBe(50)

  "Day 9" should "find the largest rectangle filled with red and green tiles only" in
    d.part2(input).shouldBe(24)
