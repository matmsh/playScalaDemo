package controllers.form

import play.api.mvc.Controller
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import play.api.data.Forms

object CustomIntMapping extends Controller {

  case class IntPair(numA: Int, numB: Int)

  implicit val intFormatter = new Formatter[Int] {
    def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Int] = {
      val value = data.get(key)
      val ans = if (value == None || value.get.isEmpty()) {
        Left(Seq(FormError(key, "form.CustomIntMapping.required.int")))
      } else {
        try {
          Right(Integer.parseInt(value.get))
        } catch {
          case ex: Exception => Left(Seq(FormError(key,
                       "form.CustomIntMapping.error.int", List(value.get))))

        }
      }
      ans
    }
    def unbind(key: String, intValue: Int) = Map(key -> intValue.toString)
    //override val format = Some(("int.format", Nil))
  }

  val intMapping = Forms.of(intFormatter)

  val pairForm = Form(
    mapping(
      "numA" -> intMapping, "numB" -> intMapping)
      (IntPair.apply)(IntPair.unapply).verifying("The difference between A and B must be divisible by 3",
              pair => (pair.numA - pair.numB) % 3 ==0)
      
  )

  def show() = Action {
    implicit request =>

      val form = if (flash.get("success").isDefined)
        this.pairForm.bind(flash.data)
      else
        this.pairForm

      Ok(views.html.form.customIntMapping(form))

  }
  
  def submit = Action {
    implicit request =>

      // formInput contains the submitted values.
      val formInput: Form[IntPair] = this.pairForm.bindFromRequest

      formInput.fold(

        // If the form has validation error, redisplay the show page with error.   
        hasErrors = { formWithError => Ok(views.html.form.customIntMapping(formWithError))},

        // If there is no validation error, redisplay the form and output entered name.
        success = { pair =>
          val newForm = pairForm.fill(pair);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.CustomIntMapping.show).flashing(flashDataMap)
        })

  }
   
}