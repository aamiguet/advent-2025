package ch.aamiguet.advent2025

class Day06 extends Day:

  case class Problem(op: Char, numbers: List[Long]):
    val result = op match
      case '*' => numbers.fold(1L)(_ * _)
      case '+' => numbers.fold(0L)(_ + _)

  private def parse(input: String): List[Problem] =
    input
      .split("\n")
      .map(_.trim().split("\\s+"))
      .transpose
      .map: ps =>
        Problem(ps.last.head, ps.dropRight(1).map(_.toLong).toList)
      .toList

  private def parseAsCephalopodMath(input: String): List[Problem] =
    val lines = input.split("\n")
    val opChunks = "(\\*|\\+)\\s+".r.findAllIn(lines.last).toList
    opChunks
      .foldLeft((List.empty[Problem], 0)):
        case ((problems, index), opChunk) =>
          val numbers = (index + opChunk.size - 1 to index by -1)
            .map: i =>
              (0 until lines.size - 1)
                .map: j =>
                  lines(j).lift(i) match
                    case None => ""
                    case Some(' ') => ""
                    case Some(c) => s"$c"
                .mkString
            .filterNot(_.isEmpty())
            .map(_.toLong)
            .toList
          (Problem(opChunk.head, numbers) :: problems, index + opChunk.size)
      ._1

  override def part1(input: String): Long =
    val problems = parse(input)
    problems.map(_.result).sum
  override def part2(input: String): Long =
    val problems = parseAsCephalopodMath(input)
    problems.map(_.result).sum

object Day06:
  val day = Day06()
  val input = data("data/day06.txt")

  @main def d06part1() = println(day.part1(input))
  @main def d06part2() = println(day.part2(input))
