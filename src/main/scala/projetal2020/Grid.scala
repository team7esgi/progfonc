package projetal2020

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
        mower.move(boundaryX, boundaryY)
      } else {
        mower.changeDirection(charac)
      }
    }
  }
}
