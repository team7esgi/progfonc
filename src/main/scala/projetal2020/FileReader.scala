package projetal2020

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json.{JsObject, Json}

import java.io.{FileNotFoundException, IOException}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Try}

object FileReader {

  @throws(classOf[FileNotFoundException])
  @throws(classOf[IOException])
  def readFile(fileName: String): List[String] = {
    val fileSource = Source.fromFile(fileName)
    val lines = fileSource.getLines().toList
    fileSource.close
    lines
  }

  @throws(classOf[DonneesIncorrectesException])
  def readInstructions(filename: String): Unit = {
    try {
      val lines: List[String] = readFile(filename)
      if (lines.size % 2 == 0) {
        throw new DonneesIncorrectesException("Le information du fichier ne sont pas correctes !")
      } else {
        val outputMower: ArrayBuffer[JsObject] = new ArrayBuffer[JsObject]()
        val lineSplit: Array[String] = lines(0).split(" ")
        val map: Grid = Grid(lineSplit(0).toInt, lineSplit(1).toInt)
        val linesSliced = lines.slice(1, lines.size).grouped(2)

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
            "fin" -> mower.info
          )
          outputMower.append(result)
        }
        printOutput(limite, outputMower)
      }
    } catch {
      case e: Throwable => throw new DonneesIncorrectesException("lawn mower direction!" + e.getMessage)
    }
  }

  def printOutput(
      limite: JsObject,
      resultMowers: ArrayBuffer[JsObject]
  ): Unit = {
    val result = Json.obj(
      "limite"    -> limite,
      "tondeuses" -> resultMowers
    )
    println(Json.prettyPrint(result))
  }
}
