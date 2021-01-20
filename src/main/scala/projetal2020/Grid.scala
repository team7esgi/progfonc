package projetal2020

import scala.util.{Failure, Success}

object Grid {
  def apply(boundX: Int, boundY: Int): Grid = {
    new Grid(boundX, boundY)
  }
}

class Grid(boundaryX: Int, boundaryY: Int) {

  def createMower(x: Int, y: Int, dir: Char): Mower = {
    new Mower(x, y, dir)
  }

  def moveMower(instructions: String, mower: Mower): Unit = {
    for (charac <- instructions.toList) {
      if (charac.equals('A')) {
        mower.getNextMove(boundaryX, boundaryY) match {
          case Success(posInfo)   => mower.updatePosition(posInfo)
          case Failure(exception) => println(exception.getMessage)
        }
      } else {
        mower.getNewDirection(charac) match {
          case Success(newDir)    => mower.updateDirection(newDir)
          case Failure(exception) => println(exception.getMessage)
        }
      }
    }
  }
}
