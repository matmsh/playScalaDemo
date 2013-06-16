package controllers.form

import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._

/**
 * An example of a form using Bootstrap.
 */
object DemoBootstrap extends Controller {

  case class Motorcycle(name: String, make: String, engineCapacity: Int,
    yearOfManufacture: Int)

  lazy val motorcycleMapping: Mapping[Motorcycle] =
    mapping(
      "name" -> nonEmptyText,
      "make" -> nonEmptyText,
      "engineCapacity" -> number(50, 1000), // capacity must be in [50,1000]
      // year of manufacture must be from 1970 to 2013 inclusively.
      "yearOfManufacture" -> number(1970, 2013))(Motorcycle.apply)(Motorcycle.unapply)

  val motorcycleForm = Form[Motorcycle](motorcycleMapping)

  def show = Action {
    implicit request =>
      val form = if ( flash.get("success").isDefined)
        // Redirect from a submit
        this.motorcycleForm.bind(flash.data)
      else{
        // initial display
        val bike = Motorcycle("CBR600", "Honda",600,2013)  
        this.motorcycleForm.fill(bike)
      }      
      Ok(views.html.form.demoBootstrap(form))
  }

  def submit = Action {
    implicit request =>
      // formInput contains the submitted values.
      val formInput: Form[Motorcycle] = this.motorcycleForm.bindFromRequest
      formInput.fold(

        // If the form has validation error, redisplay the page with error.   
        hasErrors = { formWithError =>
          Ok(views.html.form.demoBootstrap(formWithError))
        },
        // If there is no validation error, redisplay the entered form data at the bottom of the page.
        success = { motorcycle =>
          // The newForm holds the submitted data.
          val newForm = motorcycleForm.fill(motorcycle);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.DemoBootstrap.show).flashing(flashDataMap)
        })
  }
}