package controllers.form

import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._
import play.api.i18n.Messages
import play.api.data.validation.Constraint
import play.api.data.validation.Valid
import play.api.data.validation.Invalid
import play.api.data.validation.ValidationError
/**
 * An example of a form using custom error messages.
 */
object Demo2 extends Controller {

  case class Motorcycle(name: String, make: String, engineCapacity: Int,
    yearOfManufacture: Int)

  // A custom constraint.
  def rangeCheck(lower: Int, upper: Int): Constraint[Int] = {
    val constraintMsg = "Valid range %s to %s inclusively".format(lower, upper)    
    Constraint[Int](constraintMsg) { o =>
      if (o < lower || o > upper) {
        Invalid(ValidationError(Messages("form.Demo2.validation.capacity",lower,upper)))
      } else { Valid }
    }
  }
    
  lazy val motorcycleMapping: Mapping[Motorcycle] =
    mapping(
      "name" -> text.verifying(Messages("form.Demo2.validation.name"), _.nonEmpty),
      "make" -> text.verifying(Messages("form.Demo2.validation.make"), _.nonEmpty),
       // capacity must be in [50,1000]
      "engineCapacity" -> number.verifying(rangeCheck(50, 1000)),
      //   Will use a dropdown list in the form for yearOfMaufacture.
      "yearOfManufacture" -> number)(Motorcycle.apply)(Motorcycle.unapply)

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
      Ok(views.html.form.demo2(form))
  }

  def submit = Action {
    implicit request =>
      // formInput contains the submitted values.
      val formInput: Form[Motorcycle] = this.motorcycleForm.bindFromRequest
      formInput.fold(

        // If the form has validation error, redisplay the page with error.   
        hasErrors = { formWithError =>
          Ok(views.html.form.demo2(formWithError))
        },
        // If there is no validation error, redisplay the entered form data at the bottom of the page.
        success = { motorcycle =>
          // The newForm holds the submitted data.
          val newForm = motorcycleForm.fill(motorcycle);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.Demo2.show).flashing(flashDataMap)
        })
  }
}