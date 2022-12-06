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

  def day2(): Int = ???

  private def getContents(fileName: String): Seq[String] = {
    val source = Source.fromFile("inputs/" + fileName)
    val contents = source.getLines().toSeq
    source.close()
    contents
  }
}
