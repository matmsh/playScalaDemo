package controllers.form

import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._
import play.api.templates.Html


/**
 * Cross validation on a list of integers.
 */
object CrossValidation1 extends Controller {

  case class Integers(numbers: List[Int]);
  
  def validate(integers: Integers): Boolean = {
    (integers.numbers(0) - integers.numbers(2)) % 3 == 0
  }

  lazy val integersMapping: Mapping[Integers] = mapping(
    "numbers" -> list(number))(Integers.apply)(Integers.unapply).verifying(
      "The difference between the first and third entry must be divisble by 3.",
      validate _)
  

  val integersForm = Form[Integers](integersMapping)

  def show = Action {
    implicit request =>

      val form = if (flash.get("success").isDefined)
        this.integersForm.bind(flash.data)
      else
        this.integersForm

      Ok(views.html.form.crossValidation1(form))

  }
   def submit = Action {
    implicit request =>

      // formInput contains the submitted values.
      val formInput: Form[Integers] = this.integersForm.bindFromRequest

      formInput.fold(
         hasErrors = processError,
        // If there is no validation error, redisplay the form and output entered name.
        success = { integers =>
          val newForm = integersForm.fill(integers);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.CrossValidation1.show).flashing(flashDataMap)
        })

  }

  private def processError(formWithError: Form[Integers])(implicit request: Request[Any]): SimpleResult[Html] = {
    val formWithError2 = if (formWithError.hasGlobalErrors) {
      // Mark the first and third occurrence of numbers.
      formWithError.withError("numbers[0]", "*").withError("numbers[2]", "*")
    } else {
      formWithError
    }

    Ok(views.html.form.crossValidation1(formWithError2))

  }
   
   
  
}