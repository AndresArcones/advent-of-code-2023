package day2

import scala.io.Source

case class Choice(red: Int, green: Int, blue: Int) {
  def isValidChoice: Boolean = {
    if (red <= 12 && green <= 13 && blue <= 14) true else false
  }
}

case class Game(id: Int, choices: List[Choice]) {
  def isValidGame: Boolean = {
    this.choices.forall(_.isValidChoice)
  }

  def minimumGamePossible: Choice = {
    this.choices.foldLeft(Choice(0, 0, 0)) {
      (acc, choice) => {
        var result = acc
        result = if (choice.red > acc.red) result.copy(red = choice.red) else result
        result = if (choice.green > acc.green) result.copy(green = choice.green) else result
        result = if (choice.blue > acc.blue) result.copy(blue = choice.blue) else result
        result
      }
    }
  }
}

object Game {
  private def serializeChoice(choice: String): Choice = {
    val colorizedCubes = choice.split(", ")

    colorizedCubes.foldLeft(Choice(0, 0, 0)) {
      (choice, cube) => {
        val number = cube.split(" ").head.toInt
        val color = cube.split(" ").last

        color match {
          case "red" => choice.copy(red = number)
          case "green" => choice.copy(green = number)
          case "blue" => choice.copy(blue = number)
        }
      }
    }
  }

  def serializeGame(game: String): Game = {
    val gameId = game.split(": ").head.split(" ").last.toInt
    val values = game.split(": ").last

    val choices = values.split("; ")

    choices.foldLeft(Game(gameId, List.empty)) {
      (game, choice) => {
        game.copy(choices = game.choices :+ serializeChoice(choice))
      }
    }
  }
}

object part1 extends App {
  val input = Source.fromResource("input/day2/input.txt")

  val result = input.getLines().to(LazyList).map(Game.serializeGame).filter(_.isValidGame).map(_.id).sum
  println(result)

}

object part2 extends App {
  val input = Source.fromResource("input/day2/input.txt")

  val result = input.getLines().to(LazyList)
    .map(Game.serializeGame)
    .map(_.minimumGamePossible)
    .map(choice => choice.red * choice.green * choice.blue)
    .sum

  println(result)

}
