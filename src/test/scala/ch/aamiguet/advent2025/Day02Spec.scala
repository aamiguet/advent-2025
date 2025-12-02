package ch.aamiguet.advent2025

package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day02Spec extends AnyFlatSpec with should.Matchers:

  val d = Day02()

  val input =
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124""".stripMargin

  "Day 2" should "add up all the invalid IDs" in
    d.part1(input).shouldBe("1227775554")

  "Day 2" should "find and add up all invalids IDs" in
    d.part2(input).shouldBe("4174379265")
