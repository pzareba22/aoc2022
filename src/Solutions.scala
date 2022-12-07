package pzareba.aoc2022

import scala.io.Source

object Solutions {
  def day1(): (Int, Int) = {
    val contents: Seq[String] = getContents("day1.txt")

    val elfCalories = contents.foldLeft[(List[List[Int]], List[Int])](List(), List()) {
      case (acc, line) => (acc, line) match {
        case (acc, "") => (acc._1 :+ acc._2, List())
        case (acc, line) => (acc._1, acc._2 :+ line.toInt)
      }
    }._1

    val res1 = elfCalories.map(_.sum).max
    val res2 = elfCalories.map(_.sum).sorted.take(3).sum

    (res1, res2)
  }

  def day2(): (Int, Int) = {
    sealed trait Shape extends Product with Serializable
    object Shape {
      def apply(chr: Char): Shape = chr match {
        case 'A' | 'X' => Rock
        case 'B' | 'Y' => Paper
        case 'C' | 'Z' => Scissors
        case _ => throw new IllegalArgumentException("Invalid input character")
      }
    }
    case object Rock extends Shape
    case object Paper extends Shape
    case object Scissors extends Shape
    val values = Map(
      Rock -> 1,
      Paper -> 2,
      Scissors -> 3,
    )

    val wins = Map(
      Rock -> Scissors,
      Paper -> Rock,
      Scissors -> Paper,
    )

    def calculateScore(char1: Char, char2: Char): Int = {
      val shape1 = Shape(char1)
      val shape2 = Shape(char2)
      val loosingShape = wins(shape1)
      val score = shape2 match {
        case _ if shape2 == loosingShape => 0 // loss
        case _ if shape1 == shape2 => 3 // draw
        case _ => 6 // win
      }
      values(shape2) + score
    }

    val contents = getContents("day2.txt").map(_.toCharArray).map {
      case Array(c1, _, c2) => (c1, c2)
      case _ => throw new IllegalStateException("Invalid input")
    }
    val res1 = contents.map(x => calculateScore(x._1, x._2)).sum
    val res2 = contents.map {
      case (c1, c2) => (c1, c2) match {
        case (c1, 'X') => 0 + values(wins(Shape(c1))) // loss
        case (c1, 'Y') => 3 + values(Shape(c1)) // draw
        case (c1, 'Z') => 6 + values(wins.map(_.swap)(Shape(c1))) // win
      }
    }.sum

    (res1, res2)
  }

  private def getContents(fileName: String): Seq[String] = {
    val source = Source.fromFile("inputs/" + fileName)
    val contents = source.getLines().toSeq
    source.close()
    contents
  }
}
