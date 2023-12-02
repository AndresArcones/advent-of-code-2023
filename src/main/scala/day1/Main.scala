package day1

import scala.io.Source

object Helpers {

  def obtainCalibrationValue(line: String): Int = {
    val numbers = line.filter(_.isDigit)
    s"${numbers.head}${numbers.takeRight(1)}".toInt
  }

  def replaceLetterWithInt(line: String): String = {

    val wordToNumber = Map(
      "one" -> "o1e",
      "two" -> "t2o",
      "three" -> "t3e",
      "four" -> "f4r",
      "five" -> "f5e",
      "six" -> "s6x",
      "seven" -> "s7n",
      "eight" -> "e8t",
      "nine" -> "n9e",
    )

    wordToNumber.foldLeft(line.toLowerCase) { case (acc, (word, number)) =>
      acc.replace(word, number)
    }
  }
}

object part1 extends App {
    val source = Source.fromResource("input/day1/input.txt")
    val result = source.getLines().to(LazyList).map(Helpers.obtainCalibrationValue).sum
    println(result)
}

object part2 extends App{
    val input = Source.fromResource("input/day1/input.txt")
    val result = input.getLines().to(LazyList).map(Helpers.replaceLetterWithInt).map(Helpers.obtainCalibrationValue).sum
    println(result)
}
