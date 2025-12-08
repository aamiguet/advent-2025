package ch.aamiguet.advent2025

import org.scalatest.*
import flatspec.*
import matchers.*

class Day08Spec extends AnyFlatSpec with should.Matchers:

  val d = Day08()

  val input =
    """162,817,812
      |57,618,57
      |906,360,560
      |592,479,940
      |352,342,300
      |466,668,158
      |542,29,236
      |431,825,988
      |739,650,466
      |52,470,668
      |216,146,977
      |819,987,18
      |117,168,530
      |805,96,715
      |346,949,466
      |970,615,88
      |941,993,340
      |862,61,35
      |984,92,344
      |425,690,689""".stripMargin

  "Day 8" should "multiple the size of the three largest circuits" in
    d.part1(input, 10).shouldBe(40)

  "Day 8" should "multiple the x coordinate of the last two boxes connecting the whole circuit" in
    d.part2(input).shouldBe(25272)
