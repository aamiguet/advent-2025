package ch.aamiguet

import scala.io.Source

package object advent2025:
  def timing[A](block: => A): A =
    val start = System.nanoTime()
    val result = block
    val elapsed = (System.nanoTime() - start) / 1000000
    println(s"Elapsed: ${elapsed}ms")
    result

  def data(filepath: String): String =
    Source.fromFile(filepath).getLines().mkString("\n")
