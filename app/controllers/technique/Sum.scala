package controllers.technique

import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import play.Logger
import play.api.templates.Html

object Sum extends Controller {

  case class InputBean(noOfEntries:Int, entries: List[Int])

  lazy val inputForm = Form(
    mapping(
      "noOfEntries" -> number,
      "entries" -> list(number.verifying("Please enter an integer between -10 and 10 inclusively.",
        (x: Int) => x >= -10 && x <= 10)))(InputBean.apply)(InputBean.unapply))

  inputForm("noOfEntries")

  def show() = Action {
    implicit request =>
      val form = if (flash.get("sum").isDefined)
        this.inputForm.bind(flash.data)
      else if (flash.get("change").isDefined)
        this.inputForm.bind(flash.data).discardingErrors
      else {
        this.inputForm.fill(InputBean(5, List()))

      }
      Ok(views.html.technique.sum(form))

  }

  def process = Action { implicit request =>
    val actionOpt = request.body.asFormUrlEncoded.get("action").headOption
    Logger.info("Action=" + actionOpt)

    actionOpt match {
      case Some("Change") => change
      case Some("Sum") => compute
      case _ => BadRequest("This action is not allowed");
    }

  }

  /**
   *
   * If no error : Compute the sum of submitted entries.
   * If has error : Display error.
   */
  private def compute(implicit request: Request[Any]) = {

    // formInput contains the submitted values.
    val formInput: Form[InputBean] = this.inputForm.bindFromRequest
    formInput.fold(
      hasErrors = { formWithError =>
        Ok(views.html.technique.sum(formWithError))
      },
      success = { inputBean =>
        val newForm = inputForm.fill(inputBean);
        // Put submitted value and the sum of entries into flash scope.
        val flashDataMap = Flash(newForm.data) + ("sum" -> inputBean.entries.sum.toString)
        Redirect(routes.Sum.show).flashing(flashDataMap)
      })

  }

  /**
   * Redisplay the form with the newly selected no of entries.
   * The old entries are retained.
   */
  private def change(implicit request: Request[Any]) = {
    // formInput contains the submitted values.
    val formInput: Form[InputBean] = this.inputForm.bindFromRequest

    val noOfEntries = formInput.data("noOfEntries").toInt
    val entryFilter = getFilter(noOfEntries)
    val dataMapNew = formInput.data.filter(x => entryFilter(x._1))

    val flashDataMap = Flash(dataMapNew) + ("change" -> "")
    Redirect(routes.Sum.show).flashing(flashDataMap)

  }

  /**
   *  Return a filter that will retains keys like entries[i] where i <= noOfEntries and
   *  key does not start with "entries".
   */
  private def getFilter(noOfEntries: Int) = {
    val retainEntries = (0 to noOfEntries - 1).map(i => "entries[" + i + "]")
    (key: String) => !key.startsWith("entries") || retainEntries.contains(key)

  }

}







