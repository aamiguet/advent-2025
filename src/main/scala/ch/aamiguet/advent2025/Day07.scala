package ch.aamiguet.advent2025

class Day07 extends Day:

  type Manifold = Array[Array[Char]]

  private def parse(input: String): Manifold =
    input.split("\n").map(_.toArray)

  override def part1(input: String): String =
    val manifold = parse(input)
    val beamSource = Set(manifold.head.indexOf('S'))
    manifold
      .tail
      .foldLeft((beamSource, 0)):
        case ((beamIndices, splitCount), row) =>
          val splits =
            row
              .zipWithIndex
              .filter:
                case (location, i) => beamIndices(i) && location == '^'
          val updatedBeamIndices =
            beamIndices ++ splits.flatMap((_, i) => Set(i - 1, i + 1)) -- splits.map(_._2)
          (updatedBeamIndices, splitCount + splits.size)
      ._2
      .toString

  override def part2(input: String): String = ???

object Day07:
  val day = Day07()
  val input = data("data/day07.txt")

  @main def d07part1() = println(day.part1(input))
  @main def d07part2() = println(day.part2(input))
