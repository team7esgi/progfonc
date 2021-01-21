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
      case Success(instructions) => launchMower(instructions)
      case Failure(exception)    => Failure(new Exception(exception.getMessage))
    }
  }

  def launchMower(instructions: List[String]): Try[JsObject] = {
    if (instructions.size % 2 == 0) {
      Failure(
        new DonneesIncorrectesException(
          "Les informations du fichier ne sont pas correctes." + instructions.size.toString
        )
      )
    } else {
      val mowerInfoAndInstructions =
        instructions.slice(1, instructions.size).grouped(2).toList
      checkGridBoundaryInfo(instructions(0)) match {
        case Success(info) =>
          executeInstructions(info, mowerInfoAndInstructions)
        case Failure(exception) =>
          Failure(new Exception(exception.getMessage))
      }
    }
  }

  def checkGridBoundaryInfo(gridInfo: String): Try[(Int, Int)] = {
    try {
      val infos = gridInfo.trim.split(" ")
      if (infos.size > 2) {
        Failure(
          new DonneesIncorrectesException(
            "Les données des limites de la grille sont invalides."
          )
        )
      } else {
        Success((infos(0).toInt, infos(1).toInt))
      }
    } catch {
      case _: Throwable =>
        Failure(
          new DonneesIncorrectesException(
            "Les données des limites de la grille sont invalides."
          )
        )
    }
  }

  def checkMowerInitialInfo(mowerInfo: String): Try[(Int, Int, Char)] = {
    try {
      val infos = mowerInfo.trim.split(" ")
      if (infos.size > 3 || infos(2).toCharArray()(0).isDigit) {
        Failure(
          new DonneesIncorrectesException(
            "Les données de la tondeuse sont invalides."
          )
        )
      } else {
        Success(
          (
            infos(0).toInt,
            infos(1).toInt,
            infos(2).toCharArray()(0)
          )
        )
      }
    } catch {
      case _: Throwable =>
        Failure(
          new DonneesIncorrectesException(
            "Les données de la tondeuse sont invalides"
          )
        )
    }
  }

  def executeInstructions(
      gridBound: (Int, Int),
      mowerInfoAndInstructions: List[List[String]]
  ): Try[JsObject] = {
    val outputMower: ArrayBuffer[JsObject] = new ArrayBuffer[JsObject]()
    val grid: Grid = Grid(gridBound._1, gridBound._2)
    val limite = Json.obj("x" -> gridBound._1, "y" -> gridBound._2)

    for (mowerInfos <- mowerInfoAndInstructions) {
      checkMowerInitialInfo(mowerInfos(0)) match {
        case Success(info) =>
          outputMower.insert(
            outputMower.size,
            InitAndMoveMower(grid, info, mowerInfos(1))
          )
        case Failure(exception) => println(exception.getMessage)
      }
    }
    val result = Json.obj("limite" -> limite, "tondeuses" -> outputMower)
    Success(result)
  }

  def InitAndMoveMower(
      grid: Grid,
      mowerInfo: (Int, Int, Char),
      instructions: String
  ): JsObject = {
    val mower = grid.createMower(mowerInfo._1, mowerInfo._2, mowerInfo._3)
    grid.moveMower(instructions, mower)

    val result = Json.obj(
      "debut" ->
        Json.obj(
          "point" -> Json
            .obj("x" -> mowerInfo._1, "y" -> mowerInfo._2),
          "direction" -> mowerInfo._3.toString
        ),
      "instructions" -> instructions,
      "fin"          -> mower.info
    )
    result
  }
}
