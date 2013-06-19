package controllers.form

import play.api.mvc.Controller
import play.api.data.Mapping
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import play.api.data.FormError
import play.api.templates.Html

object ListOfObjectsDemo extends Controller {

  case class Points(points: List[Point])
  case class Point(x: Int, y: Int)

  lazy val pointMapping: Mapping[Point] = mapping(
    "x" -> number,
    "y" -> number)(Point.apply)(Point.unapply).verifying("x must be less than y", pt => pt.x < pt.y)

  lazy val pointsMapping: Mapping[Points] = mapping(
    "points" -> list(pointMapping))(Points.apply)(Points.unapply)

  val pointsForm = Form[Points](pointsMapping)

  def show() = Action {
    implicit request =>

      val form = if (flash.get("success").isDefined)
        this.pointsForm.bind(flash.data)
      else
        this.pointsForm

      Ok(views.html.form.listOfObjectsDemo(form))

  }

  def submit = Action {
    implicit request =>

      // formInput contains the submitted values.
      val formInput: Form[Points] = this.pointsForm.bindFromRequest

      formInput.fold(

        // If the form has validation error, redisplay the show page with error.   
        hasErrors = processError,

        // If there is no validation error, redisplay the form and output entered name.
        success = { points =>
          val newForm = pointsForm.fill(points);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.ListOfObjectsDemo.show).flashing(flashDataMap)
        })

  }

  private def processError(formWithError: Form[Points])(implicit request: Request[Any]): SimpleResult[Html] = {

    // Get the error messages for points[d]
    val pointsErrors = formWithError.errors.filter(error => error.key.matches("points\\[\\d\\]"))

    // For each key point[d], add error with key = point[d].x, and error with  key = point[d].y .
    val formWithErrorE = pointsErrors.foldLeft(formWithError) { (form: Form[Points], fError: FormError) =>
      form.withError(fError.key + ".x", "").withError(fError.key + ".y", "")
    }

    Ok(views.html.form.listOfObjectsDemo(formWithErrorE))

  }

}