package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.Play.current
import scala.collection.mutable.ListBuffer

object Source extends Controller {

  case class Display(title: String, content: String, brush: String)
  //  case class Parameters(title:String,routes:String, messages:String,
  //                 controllerSrc:String,controllerFilename:String,
  //                 viewSrc:String, viewFilename:String,
  //                 modelSrc:String,modelFilename:String)

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
      displays += Display(templateName, getFileAsStr(templateName), "xml")
    )

  
    Ok(views.html.source(title, displays.toList))
  }

  private def getFileAsStr(filename: String, filterFn: String => Boolean = (_ => true)): String = {
    val inputStream = Play.classloader.getResourceAsStream(filename)
    io.Source.fromInputStream(inputStream).getLines.filter(filterFn).mkString("\n")
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

}