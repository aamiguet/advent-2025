package ch.aamiguet.advent2025

class Day01 extends Day:

  private def positions(input: String): List[Int] =
    val rotations = input.split("\n").toList
    rotations
      .foldLeft(List(50)): (acc, rotation) =>
        val sign = rotation.head match
          case 'R' => 1
          case 'L' => -1
        val p = (acc.head + sign * rotation.tail.toInt) % 100
        p :: acc

  override def part1(input: String): Long =
    positions(input).filter(_ == 0).size

  override def part2(input: String): Long =
    val rotations = input.split("\n").toList
    val (_, count) = rotations.foldLeft((50, 0)):
      case ((pos, c), rotation) =>
        val ticks = rotation.tail.toInt
        val extra = ticks / 100
        val last = ticks % 100
        rotation.head match
          case 'R' =>
            val newPos = pos + last
            if newPos >= 100 then
              (newPos - 100, c + extra + 1)
            else
              (newPos, c + extra)
          case 'L' =>
            val newPos = pos - last
            if newPos <= 0 then
              ((newPos + 100) % 100, c + (if pos > 0 then 1 else 0) + extra)
            else
              (newPos, c + extra)
    count
  end part2

object Day01:
  val day = Day01()
  val input = data("data/day01.txt")

  @main def d01part1() = println(day.part1(input))
  @main def d01part2() = println(day.part2(input))
