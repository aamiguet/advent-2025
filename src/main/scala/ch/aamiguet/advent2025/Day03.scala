package ch.aamiguet.advent2025

class Day03 extends Day:

  private def parse(input: String): List[Vector[Int]] =
    input.split("\n").toList.map(_.map(_.asDigit).toVector)

  private def bruteForce(batteries: Vector[Int]): Int =
    val maxL = batteries.dropRight(1).max
    val posL = batteries.indexOf(maxL)
    val maxR = batteries.drop(posL + 1).max
    maxL * 10 + maxR

  private def bruteForceN(
    batteries: Vector[Int],
    n: Int,
    selected: Vector[Int] = Vector.empty[Int],
  ): Long =
    if n == 0 then selected.map(_.toString).mkString.toLong
    else
      val max = batteries.dropRight(n - 1).max
      val pos = batteries.indexOf(max)
      bruteForceN(batteries.drop(pos + 1), n - 1, selected :+ max)

  override def part1(input: String): Long =
    parse(input).map(bruteForce).sum

  override def part2(input: String): Long =
    parse(input).map(bs => bruteForceN(bs, 12)).sum

object Day03:
  val day = Day03()
  val input = data("data/day03.txt")

  @main def d03part1() = println(day.part1(input))
  @main def d03part2() = println(day.part2(input))
