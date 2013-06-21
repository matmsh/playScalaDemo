package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.Play.current
import scala.collection.mutable.ListBuffer
import play.Logger
import views.html.defaultpages.badRequest
import play.api.libs.iteratee.Enumerator

object Source extends Controller {

  case class Display(title: String, content: String, brush: String)

  /**
   * Serve given filename.
   */
  def file(filename: String) = Action {
    import play.api.Play.current

    try {
      val inputStream = Play.classloader.getResourceAsStream(filename)
      val fileContent: Enumerator[Array[Byte]] = play.api.libs.iteratee.Enumerator.fromStream(inputStream)

      SimpleResult(
        header = ResponseHeader(200),
        body = fileContent)

    } catch {
      case e: Exception =>
        BadRequest("problem reading " + filename + ". \n" + e.getMessage)
    }

  }

  /**
   *  key should be the short name of a controller object.
   */
  def show(key: String) = Action {
    val displays = ListBuffer[Display]()

    val title = current.configuration.getString(key + ".title").getOrElse("Not found")

    val filter = (x: String) => (x.contains(key))
    val routes = getFileAsStr("routes", filter) //getRoutesAsStr(key)
    displays += Display("Routes", routes, "plain")

    val messages = getFileAsStr("messages", filter)
    if (messages.nonEmpty) displays += Display("Messages", messages, "plain")

    val controllerFilename = current.configuration.getString(key + ".controller").get
    val controllerSrc = getFileAsStr(controllerFilename)
    displays += Display("Controller : " + controllerFilename, controllerSrc, "scala")

    val viewFilename = current.configuration.getString(key + ".view").get
    val viewSrc = getFileAsStr(viewFilename)
    displays += Display("View : " + viewFilename, viewSrc, "xml")

    val modelOption = current.configuration.getString(key + ".model")
    val modelSrc = modelOption.map(getFileAsStr(_)).getOrElse("")
    modelOption.foreach(m => displays += Display("Model:" + m, modelSrc, "scala"))

    val extraTemplates = getExtractTemplates(key)

    extraTemplates.foreach(templateName =>
      displays += Display(templateName, getFileAsStr(templateName), "xml"))

    val textFiles = getTextFilenames(key)
    Logger.info("textFiles=" + textFiles)

    Ok(views.html.source(title, displays.toList, textFiles))
  }

  private def getFileAsStr(filename: String, filterFn: String => Boolean = (_ => true)): String = {
    val inputStream = Play.classloader.getResourceAsStream(filename)
    try {
      io.Source.fromInputStream(inputStream).getLines.filter(filterFn).mkString("\n")
    } catch {
      case ex: Exception => {
        Logger.info("filename=" + filename + "\n" + ex.getMessage())
        ""
      }

    }

  }

  /**
   * Get extra xml templates, if there is any.
   */
  private def getExtractTemplates(key: String): List[String] = {
    val templateNames = current.configuration.getString(key + ".extraTemplates").getOrElse("")
    if (templateNames.nonEmpty)
      templateNames.split(",").toList
    else List()
  }

  /**
   * Get text file names, if there is any.
   */
  private def getTextFilenames(key: String): List[String] = {
    val filenames = current.configuration.getString(key + ".textFiles").getOrElse("")
    if (filenames.nonEmpty)
      filenames.split(",").toList
    else List()
  }

}