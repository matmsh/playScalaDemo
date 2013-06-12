package controllers.widgets

import play.api._
import play.api.mvc._
import models.AccessListProducer

object DataTablesDemo1 extends Controller {
  
  def show = Action (
      implicit request =>
   
    Ok(views.html.widgets.dataTablesDemo1(AccessListProducer.getList))
  )
  
}