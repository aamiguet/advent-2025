package ch.aamiguet.advent2025

import scala.math.*
import scala.annotation.tailrec

class Day09 extends Day:

  case class Pos(x: Long, y: Long):
    def straightDistance(other: Pos): Option[Long] =
      if this.x == other.x then Some(abs(this.y - other.y))
      else if this.y == other.y then Some(abs(this.x - other.x))
      else None

    lazy val adjacentPoints: Set[Pos] =
      (for
        i <- -1 to 1
        j <- -1 to 1 if i == 0 ^ j == 0
      yield this + Pos(i, j)).toSet

    infix def +(other: Pos): Pos =
      Pos(this.x + other.x, this.y + other.y)

  case class Segment(p1: Pos, p2: Pos):
    lazy val direction: Pos =
      if p1.x == p2.x then
        Pos(0, (p2.x - p1.x).sign)
      else
        Pos((p2.x - p1.x).sign, 0)

    lazy val sides: (Segment, Segment) =
      if p1.x == p2.x then
        val s1 = Segment(Pos(p1.x - 1, p1.y), Pos(p2.x - 1, p2.y))
        val s2 = Segment(Pos(p1.x + 1, p1.y), Pos(p2.x + 1, p2.y))
        (s1, s2)
      else
        val s1 = Segment(Pos(p1.x, p1.y - 1), Pos(p2.x, p2.y - 1))
        val s2 = Segment(Pos(p1.x, p1.y + 1), Pos(p2.x, p2.y + 1))
        (s1, s2)

    lazy val toPoints: Set[Pos] =
      if p1.x == p2.x then
        (p1.y to p2.y by (p2.y - p1.y).sign).map(y => Pos(p1.x, y)).toSet
      else
        (p1.x to p2.x by (p2.x - p1.x).sign).map(x => Pos(x, p1.y)).toSet

    lazy val length: Long = p1.straightDistance(p2).get

  case class Rectangle(c1: Pos, c3: Pos):
    lazy val area = (abs(c1.x - c3.x) + 1) * (abs(c1.y - c3.y) + 1)

    lazy val sides: Set[Segment] =
      Set(
        Segment(c1, Pos(c1.x, c3.y)),
        Segment(Pos(c1.x, c3.y), c3),
        Segment(c3, Pos(c3.x, c1.y)),
        Segment(Pos(c3.x, c1.y), c1),
      )

  case class Floor(input: String):
    lazy val redTiles: Vector[Pos] =
      input
        .split("\n")
        .map:
          case s"$x,$y" => Pos(x.toLong, y.toLong)
        .toVector

    lazy val rectangles =
      redTiles
        .toSet
        .subsets(2)
        .map(s => Rectangle(s.head, s.last))

    lazy val path: Vector[(Pos, Pos)] =
      redTiles
        .zip(redTiles.tail) :+ (redTiles.last, redTiles.head)

    lazy val pathMap = path.toMap

    lazy val pathSegments =
      path
        .map:
          case (p1, p2) => Segment(p1, p2)

    lazy val pathPoints = pathSegments.flatMap(_.toPoints).toSet

    private def computeBoundary(seed: Pos): Set[Pos] =
      val acc = scala.collection.mutable.Set[Pos]()
      val queue = scala.collection.mutable.Queue[Pos](seed)
      while queue.nonEmpty
      do
        val pos = queue.dequeue()
        acc += pos
        val toQueue =
          pos
            .adjacentPoints
            .filterNot(acc)
            .filterNot(pathPoints)
            .filter(_.adjacentPoints.exists(pathPoints))
        queue.enqueueAll(toQueue)
      println(acc.size)
      acc.toSet

    lazy val exteriorBoundary: Set[Pos] =
      pathPoints
        .head
        .adjacentPoints
        .filterNot(pathPoints)
        .toList
        .map(p => computeBoundary(p))
        .sortWith(_.map(_.x).max > _.map(_.x).max)
        //.tapEach(println)
        .head

    @tailrec
    private def inbound(pos: Pos, direction: Pos, steps: Long): Boolean =
      if steps <= 0 then true
      else
        val nextPos = pos + direction
        if exteriorBoundary(nextPos) then false
        else inbound(nextPos, direction, steps - 1)

    def inbound(segment: Segment): Boolean =
      inbound(segment.p1, segment.direction, segment.length)

    def inbound(rect: Rectangle): Boolean =
      rect.sides.forall(inbound)

  override def part1(input: String): Long =
    Floor(input)
      .rectangles
      .toList
      .sortWith(_.area > _.area)
      .head
      .area

  override def part2(input: String): Long =
    val floor = Floor(input)
    Floor(input)
      .rectangles
      .toList
      .sortWith(_.area > _.area)
      .filter(floor.inbound(_))
      .head
      .area

object Day09:
  val day = Day09()
  val input = data("data/day09.txt")

  @main def d09part1() = println(day.part1(input))
  @main def d09part2() = println(day.part2(input))
