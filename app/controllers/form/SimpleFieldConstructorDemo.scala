package controllers.form

import play.api.mvc.Controller
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

object SimpleFieldConstructorDemo extends Controller {

  case class Item(itemName: String, description: String)

  val itemForm = Form(
    mapping(
      "itemName" -> text.verifying("Item name is required.", _.nonEmpty).verifying(
                    "Item name must be at least 5 chars long.", _.length > 4),
      "description" -> text.verifying("Description is required.", _.nonEmpty))
      (Item.apply)(Item.unapply))

  def show() = Action {
    implicit request =>

      val form = if (flash.get("success").isDefined)
        this.itemForm.bind(flash.data)
      else
        this.itemForm

      Ok(views.html.form.simpleFieldConstructorDemo(form))

  }

  def submit = Action {
    implicit request =>

      // formInput contains the submitted values.
      val formInput: Form[Item] = this.itemForm.bindFromRequest

      formInput.fold(

        // If the form has validation error, redisplay the show page with error.   
        hasErrors = { formWithError => Ok(views.html.form.simpleFieldConstructorDemo(formWithError)) },

        // If there is no validation error, redisplay the form and output entered name.
        success = { item =>
          val newForm = itemForm.fill(item);
          val flashDataMap = Flash(newForm.data) + ("success" -> "Added")
          Redirect(routes.SimpleFieldConstructorDemo.show).flashing(flashDataMap)
        })

  }

}