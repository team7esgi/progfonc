package projetal2020

import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success}

object Main extends App {
  FileReader.readInstructions(
    "/home/thesaint/Bureau/projet_scala/progfun-al-2020/src/main/scala/projetal2020/Instructions"
  ) match {
    case Success(n) => printOutput(n)
    case Failure(e) => print(e.getMessage)
  }

  def printOutput(result: JsObject): Unit = {
    println(Json.prettyPrint(result))
  }
}
