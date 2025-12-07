package ch.aamiguet.advent2025

class Day07Alt extends Day:

  type Manifold = (source: Int, splitterRows: List[Set[Int]])

  private def parse(input: String): Manifold =
    val lines = input.split("\n")
    val source = lines.head.indexOf('S')
    val rows =
      lines
        .tail
        .map:
          _
            .zipWithIndex
            .filter: (location, _) =>
              location == '^'
            .map(_._2)
            .toSet
        .toList
    (source, rows)

  override def part1(input: String): Long =
    val manifold = parse(input)
    manifold
      .splitterRows
      .foldLeft(Set(manifold.source), 0):
        case ((beamIndices, splitCount), row) =>
          val updatedBeamIndices = beamIndices.flatMap: i =>
            if row(i) then Set(i - 1, i + 1)
            else Set(i)
          (updatedBeamIndices, splitCount + row.filter(beamIndices).size)
      ._2

  override def part2(input: String): Long =
    val manifold = parse(input)
    manifold
      .splitterRows
      .foldLeft(Map(manifold.source -> 1L)): (tachyonTimelines, row) =>
        tachyonTimelines
          .toList
          .flatMap:
            case (i, count) =>
              if row(i) then List((i - 1) -> count, (i + 1) -> count)
              else List(i -> count)
          .groupMapReduce(_._1)(_._2)(_ + _)
      .values
      .sum

object Day07Alt:
  val day = Day07Alt()
  val input = data("data/day07.txt")

  @main def d07Altpart1() = println(day.part1(input))
  @main def d07Altpart2() = println(day.part2(input))
