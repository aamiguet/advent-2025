package ch.aamiguet.advent2025

class Day10 extends Day:

  type Lights = String
  type Button = Set[Int]

  case class Machine(
    lightsGoal: Lights,
    buttons: Set[Button],
    joltagesGoal: List[Int],
  ):
    private val initLights = "." * lightsGoal.size
    private val presses = scala.collection.mutable.Map[Lights, Int](initLights -> 0)

    private def pressButton(
      lights: Lights,
      button: Button,
    ): Lights =
      val arr = lights.toCharArray()
      button.foreach(b => arr.update(b, if arr(b) == '.' then '#' else '.'))
      arr.mkString

    private def compute(ls: List[Lights] = List(initLights), n: Int = 1): Unit =
      val nextLights = (for
        l <- ls
        b <- buttons
        next = pressButton(l, b)
        if !presses.contains(next)
      yield pressButton(l, b))
      presses.addAll(nextLights.map(l => (l, n)))
      if nextLights.nonEmpty then compute(nextLights, n + 1)

    def configureLights: Int =
      compute()
      presses(lightsGoal)

    private def pressButtonJoltage(
      state: Vector[Int],
      button: Button,
    ): Vector[Int] =
      state
        .zipWithIndex
        .map: (j, i) =>
          if button(i) then j + 1
          else j

    def configureJoltage: Int =
      def loop(states: List[Vector[Int]] = List(Vector.fill(joltagesGoal.size)(0)), n: Int = 0): Int =
        if states.exists(_.lazyZip(joltagesGoal).forall((a, b) => a == b)) then n
        else
          val nextStates = for
            s <- states
            b <- buttons
            next = pressButtonJoltage(s, b)
            if next.lazyZip(joltagesGoal).forall((a, b) => a <= b)
          yield next
          loop(nextStates, n + 1)
      loop()

  object Machine:
    def apply(line: String): Machine = line match
      case s"[$lights] $buttons {$joltages}" =>
        Machine(
          lights,
          buttons
            .split(" ")
            .map:
              case s"($ids)" => ids.split(",").map(_.toInt).toSet
            .toSet,
          joltages.split(",").map(_.toInt).toList,
        )

  private def parse(input: String) =
    input.split("\n").map(Machine.apply).toList

  override def part1(input: String): Long =
    val machines = parse(input)
    machines.map(_.configureLights).sum

  override def part2(input: String): Long =
    val machines = parse(input)
    machines
      .zipWithIndex
      .map: (m, i) =>
        println(s"Processing machine $i")
        m.configureJoltage
      .sum

object Day10:
  val day = Day10()
  val input = data("data/day10.txt")

  @main def d10part1() = timing(println(day.part1(input)))
  @main def d10part2() = println(day.part2(input))
