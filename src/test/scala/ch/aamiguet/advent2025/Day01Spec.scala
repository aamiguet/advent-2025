package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day01Spec extends AnyFlatSpec with should.Matchers:

  val d = Day01()

  val input =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin

  "Day 1" should "compute the number of times the dial points at 0" in {
    d.part1(input).shouldBe(3)
  }

  "Day 1" should "compute the number of times the dial points at 0 at any time in a rotation" in {
    d.part2(input).shouldBe(6)
  }
