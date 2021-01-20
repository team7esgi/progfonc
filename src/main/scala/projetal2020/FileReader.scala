package projetal2020

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.{JsObject, Json}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object FileReader {

  def readFile(fileName: String): Try[List[String]] = {
    try {
      val fileSource = Source.fromFile(fileName)
      val lines = fileSource.getLines().toList
      fileSource.close
      Success(lines)
    } catch {
      case exception: Throwable => Failure(new Exception(exception.getMessage))
    }
  }

  def readInstructions(fileName: String): Try[JsObject] = {
    readFile(fileName) match {
      case Success(n) => executeInstructions(n)
      case Failure(e) => Failure(new Exception(e.getMessage))
    }
  }

  def executeInstructions(instructions: List[String]): Try[JsObject] = {

    if (instructions.size % 2 == 0) {
      Failure(
        new DonneesIncorrectesException(
          "Le information du fichier ne sont pas correctes !"
        )
      )
    } else {
      val outputMower: ArrayBuffer[JsObject] = new ArrayBuffer[JsObject]()
      val lineSplit: Array[String] = instructions(0).split(" ")
      val map: Grid = Grid(lineSplit(0).toInt, lineSplit(1).toInt)
      val linesSliced = instructions.slice(1, instructions.size).grouped(2)

      val limite = Json.obj("x" -> lineSplit(0), "y" -> lineSplit(1))

      for (mowerInstructions <- linesSliced) {
        //println(mowerInstructions(0) + " : " + mowerInstructions(1))
        val mowerInfo = mowerInstructions(0).split(" ")
        val mower = map.createMower(
          mowerInfo(0).toInt,
          mowerInfo(1).toInt,
          mowerInfo(2).toCharArray()(0)
        )
        map.moveMower(mowerInstructions(1), mower)

        val result = Json.obj(
          "debut" ->
            Json.obj(
              "point" -> Json
                .obj("x" -> mowerInfo(0).toInt, "y" -> mowerInfo(1).toInt),
              "direction" -> mowerInfo(2).toCharArray()(0).toString
            ),
          "instructions" -> mowerInstructions(1),
          "fin"          -> mower.info
        )
        outputMower.append(result)
      }
      val result = Json.obj("limite" -> limite, "tondeuses" -> outputMower)
      Success(result)
    }
  }
}
