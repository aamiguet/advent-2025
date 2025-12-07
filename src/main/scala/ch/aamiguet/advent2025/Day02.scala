package ch.aamiguet.advent2025

class Day02 extends Day:

  case class IdRange(lowerBound: String, upperBound: String):
    private val lb = lowerBound.toLong
    private val ub = upperBound.toLong
    private val lbSize = lowerBound.size
    private val ubSize = upperBound.size

    def invalidIds: List[Long] =
      val l = lowerBound.take(math.max(lbSize / 2, 1)).toLong
      val u = upperBound.take((ubSize / 2d).ceil.toInt).toLong
      (l to u)
        .map: n =>
          s"$n$n".toLong
        .filter: id =>
          id >= lb && id <= ub
        .toList

    def invalidIds(window: Int, repeat: Int): Set[Long] =
      val l = 1
      val u = (s"9" * window).toInt
      (l to u)
        .map: n =>
          (s"$n" * repeat).toLong
        .filter: id =>
          lb <= id && id <= ub
        .toSet

    def allInvalidIds: Set[Long] =
      for
        window <- (1 to (ubSize / 2d).ceil.toInt).toSet
        repeat <- lbSize / window to ubSize / window if repeat >= 2
        ids <- invalidIds(window, repeat)
      yield ids

  private def parse(input: String): List[IdRange] =
    input.split(",").toList.map:
      case s"$lower-$upper" => IdRange(lower, upper)

  override def part1(input: String): Long =
    val idRanges = parse(input)
    idRanges.flatMap(_.invalidIds).sum

  override def part2(input: String): Long =
    val idRanges = parse(input)
    idRanges.flatMap(_.allInvalidIds).sum

object Day02:
  val day = Day02()
  val input = data("data/day02.txt")

  @main def d02part1() = println(day.part1(input))
  @main def d02part2() = println(day.part2(input))
