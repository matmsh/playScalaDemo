package controllers

import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.Play.current

object Source extends Controller {

  case class Parameters(title:String,routes:String, messages:String,
                 controllerSrc:String,controllerFilename:String,
                 viewSrc:String, viewFilename:String,
                 modelSrc:String,modelFilename:String)
  
  /**
   *  key should be the short name of a controller object.  
   */
  def show(key: String) = Action {

    val title = current.configuration.getString(key + ".title").getOrElse("Not found")
      
    val controllerFilename = current.configuration.getString(key + ".controller").get
    val controllerSrc = getFileAsStr(controllerFilename)

    val viewFilename = current.configuration.getString(key + ".view").get
    val viewSrc = getFileAsStr(viewFilename)
    
  
    val modelOption = current.configuration.getString(key + ".model")
    val modelSrc  = modelOption.map( getFileAsStr(_)).getOrElse("")
    val modelFilename = modelOption.getOrElse("");  
    
    val filter = (x:String) => (x.contains(key))
    
    val routes =  getFileAsStr("routes", filter) //getRoutesAsStr(key)
    val messages =getFileAsStr("messages", filter)
    
     
    val params = Parameters(title,routes,messages,controllerSrc,controllerFilename,
                     viewSrc, viewFilename,modelSrc,modelFilename)
     Ok(views.html.source(params))
  } 


   
  private def getFileAsStr(filename:String,filterFn: String => Boolean= (_ => true) ):String={
     val inputStream = Play.classloader.getResourceAsStream(filename)
     io.Source.fromInputStream(inputStream).getLines.filter( filterFn).mkString("\n")
  }
  

}