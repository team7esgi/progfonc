package projetal2020

import scala.collection.mutable
import play.api.libs.json.{JsObject, Json}

class Mower(x: Int, y: Int, dir: Char) {

  val coords: mutable.HashMap[String, Int] = new mutable.HashMap()
  val direction: mutable.HashMap[String, Char] = new mutable.HashMap()
  coords("x") = x
  coords("y") = y
  direction("dir") = dir

  @throws(classOf[DonneesIncorrectesException])
  def move(boundaryX: Int, boundaryY: Int): Unit = {
    direction("dir") match {
      case 'N' => // Nord
        if (coords("y") + 1 <= boundaryY) {
          coords("y") += 1
        } else {
          println("out of the map in north")
        }
      case 'E' => //  Est
        if (coords("x") + 1 <= boundaryX) {
          coords("x") += 1
        } else {
          println("out of the map in east")
        }
      case 'W' => // Ouest
        if (coords("x") - 1 >= 0) {
          coords("x") -= 1
        } else {
          println("out of the map in west")
        }
      case 'S' => // Sud
        if (coords("y") - 1 >= 0) {
          coords("y") -= 1
        } else {
          println("out of the map in south")
        }
    }
  }

  @throws(classOf[DonneesIncorrectesException])
  def changeDirection(instruction: Char): Unit = {
    direction("dir") match {
      case 'N' =>
        if (instruction.equals('G')) {
          direction("dir") = 'W'
        } else {
          direction("dir") = 'E'
        }
      case 'E' =>
        if (instruction.equals('G')) {
          direction("dir") = 'N'
        } else {
          direction("dir") = 'S'
        }
      case 'W' =>
        if (instruction.equals('G')) {
          direction("dir") = 'S'
        } else {
          direction("dir") = 'N'
        }
      case 'S' =>
        if (instruction.equals('G')) {
          direction("dir") = 'E'
        } else {
          direction("dir") = 'W'
        }
      case _ => throw new DonneesIncorrectesException("Error in the lawn mower direction!")
    }
  }

  override def toString: String = {
    //"x : " + coords("x").toString + ", y : " + coords("y").toString + "dir : " + direction("dir").toString
    info.toString()
  }

  lazy val info: JsObject = Json.obj(
    "point" -> Json
      .obj("x" -> coords("x").toString, "y" -> coords("y").toString),
    "direction" -> direction("dir").toString
  )
}
