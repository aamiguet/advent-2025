package ch.aamiguet.advent2025

class Day05 extends Day:

  type Range = (from: Long, to: Long)

  extension (r: Range)
    def contains(id: Long) = r.from <= id && id <= r.to

  given Ordering[Range] = new Ordering[Range]:
    def compare(x: (from: Long, to: Long), y: (from: Long, to: Long)): Int =
      if x.from == y.from then
        y.to.compare(x.to)
      else
        x.from.compare(y.from)

  private def parse(input: String): (List[Range], List[Long]) =
    val Array(ranges, ids) = input.split("\n\n")
    (
      ranges.split("\n").toList.map:
        case s"$from-$to" => (from.toLong, to.toLong),
      ids.split("\n").toList.map(_.toLong),
    )

  override def part1(input: String): Long =
    val (ranges, ids) = parse(input)
    ids.filter(id => ranges.exists(_.contains(id))).size

  private def merge(range: Range, merged: List[Range]): List[Range] =
    // bigger ranges are sorted before smaller one so it's already merged
    if range.from == merged.head.from then
      merged
    // merged range is bigger
    else if range.to <= merged.head.to then
      merged
    // overlapping ranges, we extends
    else if range.from <= merged.head.to then
      (merged.head.from, range.to) :: merged.tail
    // we add a new unique range
    else
      range :: merged

  override def part2(input: String): Long =
    val (ranges, _) = parse(input)
    val srs = ranges.sorted
    val uniqueRanges = srs.tail.foldLeft(List(srs.head)): (acc, range) =>
      merge(range, acc)
    uniqueRanges.map(r => (r.to - r.from) + 1).sum

object Day05:
  val day = Day05()
  val input = data("data/day05.txt")

  @main def d05part1() = println(day.part1(input))
  @main def d05part2() = println(day.part2(input))
