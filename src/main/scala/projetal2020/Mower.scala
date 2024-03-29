package projetal2020

import scala.collection.mutable
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

class Mower(x: Int, y: Int, dir: Char) {

  val mowerInfo: mutable.HashMap[String, Any] =
    new mutable.HashMap[String, Any]()
  mowerInfo("x") = x
  mowerInfo("y") = y
  mowerInfo("dir") = dir

  def updatePosition(newPosInfo: (String, Int)): Unit = {
    mowerInfo(newPosInfo._1) = mowerInfo(newPosInfo._1).toString.toInt + newPosInfo._2
  }

  def updateDirection(newDirection: Char): Unit = {
    mowerInfo("dir") = newDirection
  }

  def getNextMove(boundaryX: Int, boundaryY: Int): Try[(String, Int)] = {
    mowerInfo("dir") match {
      case 'N' => // Nord
        if (mowerInfo("y").toString.toInt + 1 <= boundaryY) {
          Success(("y", 1))
        } else {
          Failure(new OutOfGridException("out of the map in north"))
        }
      case 'E' => //  Est
        if (mowerInfo("x").toString.toInt + 1 <= boundaryX) {
          Success(("x", 1))
        } else {
          Failure(new OutOfGridException("out of the map in east"))
        }
      case 'W' => // Ouest
        if (mowerInfo("x").toString.toInt - 1 >= 0) {
          Success(("x", -1))
        } else {
          Failure(new OutOfGridException("out of the map in west"))
        }
      case 'S' => // Sud
        if (mowerInfo("y").toString.toInt - 1 >= 0) {
          Success(("y", -1))
        } else {
          Failure(new OutOfGridException("out of the map in south"))
        }
      case _ =>
        Failure(
          new DonneesIncorrectesException("Error in the mower direction!")
        )
    }
  }

  def getNewDirection(instruction: Char): Try[Char] = {
    mowerInfo("dir") match {
      case 'N' =>
        if (instruction.equals('G')) {
          Success('W')
        } else if (instruction.equals('D')) {
          Success('E')
        } else {
          Failure(
            new DonneesIncorrectesException("Mauvaise instruction de direction")
          )
        }
      case 'E' =>
        if (instruction.equals('G')) {
          Success('N')
        } else if (instruction.equals('D')) {
          Success('S')
        } else {
          Failure(
            new DonneesIncorrectesException("Mauvaise instruction de direction")
          )
        }
      case 'W' =>
        if (instruction.equals('G')) {
          Success('S')
        } else if (instruction.equals('D')) {
          Success('N')
        } else {
          Failure(
            new DonneesIncorrectesException("Mauvaise instruction de direction")
          )
        }
      case 'S' =>
        if (instruction.equals('G')) {
          Success('E')
        } else if (instruction.equals('D')) {
          Success('W')
        } else {
          Failure(
            new DonneesIncorrectesException("Mauvaise instruction de direction")
          )
        }
      case _ =>
        Failure(
          new DonneesIncorrectesException("Error in the mower direction!")
        )
    }
  }

  override def toString: String = {
    //"x : " + coords("x").toString + ", y : " + coords("y").toString + "dir : " + direction("dir").toString
    info.toString()
  }

  lazy val info: JsObject = Json.obj(
    "point" -> Json
      .obj("x" -> mowerInfo("x").toString, "y" -> mowerInfo("y").toString),
    "direction" -> mowerInfo("dir").toString
  )
}
