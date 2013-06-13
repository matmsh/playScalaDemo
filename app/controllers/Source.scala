package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.Play.current

object Source extends Controller {

  /**
   *  key should be the short name of a controller object.  
   */
  def show(key: String) = Action {

    val title = current.configuration.getString(key + ".title").getOrElse("Not found")
      
    val controllerFilename = current.configuration.getString(key + ".controller").get
    val controllerSrc = getFileAsString(controllerFilename)

    val viewFilename = current.configuration.getString(key + ".view").get
    val viewSrc = getFileAsString(viewFilename)
    
    //val modelFilename = current.configuration.getString(key + ".model").get
   // val modelSrc = getFileAsString(modelFilename)

    val modelOption = current.configuration.getString(key + ".model")
    val modelSrc  = modelOption.map( getFileAsString(_)).getOrElse("")
    val modelFilename = modelOption.getOrElse("");  
    
    
    val routes = getRoutesAsStr(key)

    Ok(views.html.source(title,routes, controllerSrc,controllerFilename, viewSrc,
         viewFilename, modelSrc,modelFilename))
  }

  private def getFileAsString(filename: String): String = {
    val inputStream = Play.classloader.getResourceAsStream(filename)
    io.Source.fromInputStream(inputStream).getLines.mkString("\n")
  }
  
  private def getRoutesAsStr(key: String): String = {
    val inputStream = Play.classloader.getResourceAsStream("routes")
    io.Source.fromInputStream(inputStream).getLines.filter( _.contains(key)).mkString("\n")

  }

}