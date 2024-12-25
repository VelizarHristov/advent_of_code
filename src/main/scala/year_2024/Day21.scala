package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day21(): Unit = {
  val numpad = List("789", "456", "123", "!0A").zipWithIndex.flatMap((str, y) => {
    str.zipWithIndex.map((c, x) => ((x, y), c)).filter(_._2 != '!')
  }).toMap
  val controls = List("!^A", "<v>").zipWithIndex.flatMap((str, y) => {
    str.zipWithIndex.map((c, x) => ((x, y), c)).filter(_._2 != '!')
  }).toMap
  val controlToDir = Map('^' -> (0, -1), '<' -> (-1, 0), 'v' -> (0, 1), '>' -> (1, 0))
  trait PressResult
  case object SuccessfulPress extends PressResult
  case object InvalidPress extends PressResult
  case class PressedNumpad(c: Char) extends PressResult
  trait Keyboard {
    def press(pos: (Int, Int)): (PressResult, Keyboard)
    def validPos(pos: (Int, Int)): Boolean
  }
  case class Robot(x: Int, y: Int, keyboard: Keyboard) {
    def pos = (x, y)
    def move(dir: (Int, Int)): Option[Robot] = {
      if (keyboard.validPos(pos + dir))
        Some(Robot(x + dir._1, y + dir._2, keyboard))
      else
        None
    }
    def pressKeyboard: (PressResult, Robot) = {
      val (res, newKeyboard) = keyboard.press(pos)
      (res, Robot(x, y, newKeyboard))
    }
  }
  case object DoorKeyboard extends Keyboard {
    def press(pos: (Int, Int)) = numpad.get(pos) match {
      case Some(c) => (PressedNumpad(c), this)
      case None => throw new IllegalStateException(pos.toString)
    }
    def validPos(pos: (Int, Int)) = numpad.contains(pos)
  }
  case class RobotController(robot: Robot) extends Keyboard {
    def press(pos: (Int, Int)) = controls.get(pos) match {
      case Some('A') =>
        val (res, newRobot) = robot.pressKeyboard
        (res, RobotController(newRobot))
      case Some(arrow) =>
        robot.move(controlToDir(arrow)) match {
          case Some(newRobot) => (SuccessfulPress, RobotController(newRobot))
          case None => (InvalidPress, this)
        }
      case None => (InvalidPress, this)
    }
    def validPos(pos: (Int, Int)) = controls.contains(pos)
  }

  var initialState: Keyboard = RobotController(
    Robot(2, 0, RobotController(
      Robot(2, 0, RobotController(
        Robot(2, 3, DoorKeyboard))))))
  val res = Source.fromFile("resources/2024/21").getLines.map(code => {
    var digitsTyped = 0
    var totalMoves = 0
    var states = Vector(initialState)
    while (digitsTyped < code.length) {
      var foundState: Option[Keyboard] = None
      var moves = 0
      val visited = mutable.Set(states.head)
      while (foundState == None) {
        states = for {
          state <- states
          (keyPos, _) <- controls
          (res, nextState) = state.press(keyPos)
          nextStateFiltered <- res match {
            case SuccessfulPress  => Some(nextState)
            case InvalidPress     => None
            case PressedNumpad(c) =>
              if (c == code(digitsTyped))
                foundState = Some(nextState)
              None
          }
          if !visited(nextState)
        } yield nextStateFiltered
        visited ++= states
        moves += 1
      }
      states = Vector(foundState.get)
      totalMoves += moves
      digitsTyped += 1
      initialState = foundState.get
    }

    code.take(3).toInt * totalMoves
  }).sum

  println(res)
}
