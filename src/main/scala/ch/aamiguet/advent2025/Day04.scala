package ch.aamiguet.advent2025

class Day04 extends Day:

  type Pos = (x: Int, y: Int)

  type Grid = Array[Array[Char]]

  private def parse(input: String): Grid =
    input.split("\n").map(_.toArray)

  private def accessibleRolls(grid: Grid): List[Pos] =
    def accessible(pos: Pos) =
      val (x, y) = pos
      val isRoll = grid(x)(y) == '@'
      lazy val adjacentRolls =
        for
          i <- -1 to 1
          j <- -1 to 1 if !(i == 0 && j == 0)
        yield grid.lift(x + i).flatMap(_.lift(y + j)).map(_ == '@').getOrElse(false)
      isRoll && adjacentRolls.filter(identity).size < 4

    for
      x <- (0 until grid.length).toList
      y <- 0 until grid(x).length if accessible(x, y)
    yield (x, y)

  private def removeRolls(grid: Grid): Int =
    def loop(acc: Int): Int =
      val ars = accessibleRolls(grid)
      if ars.isEmpty then acc
      else
        ars.foreach: (x, y) =>
          grid(x).update(y, '.')
        loop(acc + ars.size)
    loop(0)

  override def part1(input: String): Long =
    val grid = parse(input)
    accessibleRolls(grid).size
  override def part2(input: String): Long =
    val grid = parse(input)
    removeRolls(grid)

object Day04:
  val day = Day04()
  val input = data("data/day04.txt")

  @main def d04part1() = println(day.part1(input))
  @main def d04part2() = println(day.part2(input))
