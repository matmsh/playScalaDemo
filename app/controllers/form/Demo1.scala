package controllers.form

import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._

/**
 * An example of a form using default error messages.
 */
object Demo1 extends Controller {

  case class Motorcycle(name: String, make: String, engineCapacity: Int,
    yearOfManufacture: Int)

  lazy val motorcycleMapping: Mapping[Motorcycle] =
    mapping(
      "name" -> nonEmptyText,
      "make" -> nonEmptyText,
      "engineCapacity" -> number(50, 1000), // capacity must be in [50,1000]
      // Will use a dropdown list in the form for yearOfMaufacture.
      "yearOfManufacture" -> number)(Motorcycle.apply)(Motorcycle.unapply)

  val motorcycleForm = Form[Motorcycle](motorcycleMapping)

  def show = Action {
    implicit request =>
      val form = if ( flash.get("success").isDefined)
        // Redirect from a submit
        this.motorcycleForm.bind(flash.data)
      else{
        // initial display (do not set year of manufacture.)
        val bike = Motorcycle("CBR600", "Honda",600,0)  
        this.motorcycleForm.fill(bike)
      }      
      Ok(views.html.form.demo1(form))
  }

  def submit = Action {
    implicit request =>
      // formInput contains the submitted values.
      val formInput: Form[Motorcycle] = this.motorcycleForm.bindFromRequest
      formInput.fold(

        // If the form has validation error, redisplay the page with error.   
        hasErrors = { formWithError =>
          Ok(views.html.form.demo1(formWithError))
        },
        // If there is no validation error, redisplay the entered form data at the bottom of the page.
        success = { motorcycle =>
          // The newForm holds the submitted data.
          val newForm = motorcycleForm.fill(motorcycle);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.Demo1.show).flashing(flashDataMap)
        })
  }
}