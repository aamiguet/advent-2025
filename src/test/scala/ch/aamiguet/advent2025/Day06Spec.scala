package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day06Spec extends AnyFlatSpec with should.Matchers:

  val d = Day06()

  val input =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +  """.stripMargin

  "Day 6" should "compute the grand total" in
    d.part1(input).shouldBe(4277556)

  "Day 6" should "compute the grand total in cephalopod math" in
    d.part2(input).shouldBe(3263827)
