package ch.aamiguet.advent2025

class Day08 extends Day:

  type Pos = (x: Int, y: Int, z: Int)

  extension (p: Pos)
    def distance(other: Pos) =
      math.sqrt(math.pow(p.x - other.x, 2) + math.pow(p.y - other.y, 2) + math.pow(p.z - other.z, 2))

  extension [A](s: Set[A])
    def pop(f: A => Boolean): (Option[A], Set[A]) =
      s.find(f) match
        case Some(a) => (Some(a), s - a)
        case None => (None, s)

  private def parse(input: String): Set[Pos] =
    input
      .split("\n")
      .map:
        case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt)
      .toSet

  private def distances(boxes: Set[Pos]): List[(Set[Pos], Double)] =
    boxes
      .subsets(2)
      .map:
        case s => s -> s.head.distance(s.last)
      .toList

  private def connect(circuits: Set[Set[Pos]], pairOfBoxes: Set[Pos]): Set[Set[Pos]] =
    val (o1, c1) = circuits.pop(_(pairOfBoxes.head))
    val (o2, c2) = c1.pop(_(pairOfBoxes.last))
    val toAdd = (o1, o2) match
      // connecting two circuits
      case (Some(s1), Some(s2)) => s1 ++ s2
      // or adding a box to an existing circuit
      case (Some(s1), None) => s1 + pairOfBoxes.last
      case (None, Some(s2)) => s2 + pairOfBoxes.head
      // creating a new circuit
      case _ => pairOfBoxes
    c2 + toAdd

  def part1(input: String, connections: Int): Long =
    val boxes = parse(input)
    distances(boxes)
      .sortBy(_._2)
      .map(_._1)
      .take(connections)
      .foldLeft(Set.empty[Set[Pos]])(connect)
      .map(_.size)
      .toList
      .sorted(using Ordering[Int].reverse)
      .take(3)
      .product

  override def part1(input: String): Long =
    part1(input, 1000)

  private def connectUntilComplete(
    pairsOfBoxes: List[Set[Pos]],
    size: Int,
    circuits: Set[Set[Pos]] = Set.empty[Set[Pos]],
  ): Long =
    val pairOfBoxes = pairsOfBoxes.head
    val newCircuits = connect(circuits, pairOfBoxes)
    if newCircuits.size == 1 && newCircuits.head.size == size then
      pairOfBoxes.head.x.toLong * pairOfBoxes.last.x.toLong
    else
      connectUntilComplete(pairsOfBoxes.tail, size, newCircuits)

  override def part2(input: String): Long =
    val boxes = parse(input)
    val allPairsOfBoxes = distances(boxes)
      .sortBy(_._2)
      .map(_._1)
    connectUntilComplete(allPairsOfBoxes, boxes.size)

object Day08:
  val day = Day08()
  val input = data("data/day08.txt")

  @main def d08part1() = println(day.part1(input))
  @main def d08part2() = println(day.part2(input))
