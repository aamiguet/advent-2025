package ch.aamiguet.advent2025

class Day07 extends Day:

  type Manifold = Array[String]

  private def parse(input: String): Manifold =
    input.split("\n")

  private def findSplitIndices(row: String, beamIndices: Set[Int]): List[Int] =
    row
      .zipWithIndex
      .filter: (location, i) =>
        beamIndices(i) && location == '^'
      .map(_._2)
      .toList

  override def part1(input: String): Long =
    val manifold = parse(input)
    val beamSource = Set(manifold.head.indexOf('S'))
    manifold
      .tail
      .foldLeft((beamSource, 0)):
        case ((beamIndices, splitCount), row) =>
          val splitIndices = findSplitIndices(row, beamIndices)
          val updatedBeamIndices =
            beamIndices ++ splitIndices.flatMap(i => Set(i - 1, i + 1)) -- splitIndices
          (updatedBeamIndices, splitCount + splitIndices.size)
      ._2

  override def part2(input: String): Long =
    val manifold = parse(input)
    val beamTimelineSource = Map(manifold.head.indexOf('S') -> 1L)
    manifold
      .tail
      .foldLeft(beamTimelineSource): (beamTimelines, row) =>
        val splitIndices = findSplitIndices(row, beamTimelines.keySet)
        val splitTimelines =
          splitIndices
            .flatMap: i =>
              val pastTimelines = beamTimelines(i)
              List((i + 1) -> pastTimelines, (i - 1) -> pastTimelines)
            .groupMap(_._1)(_._2)
            .view
            .mapValues(_.sum)
            .toMap
        val updatedBeamTimelines =
          splitTimelines
            .foldLeft(beamTimelines): (bm, s) =>
              bm.updatedWith(s._1):
                case None => Some(s._2)
                case Some(n) => Some(n + s._2)
            .removedAll(splitIndices)
        updatedBeamTimelines
      .values
      .sum

object Day07:
  val day = Day07()
  val input = data("data/day07.txt")

  @main def d07part1() = println(day.part1(input))
  @main def d07part2() = println(day.part2(input))
