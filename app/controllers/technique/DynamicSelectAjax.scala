package controllers.technique

import play.api.mvc._
import play.api._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

object DynamicSelectAjax extends Controller {

  trait OptionModel[E] {
    def label: String;
    def value: E
  }

  trait SelectModel[E] {
    def options: List[OptionModel[E]]
  }

  case class DefaultOptionModel[E](label: String, value: E) extends OptionModel[E]
  case class DefaultSelectModel[E](options: List[OptionModel[E]]) extends SelectModel[E]

  case class Motorcycle(make: String, model: String)

  val makes = List("BMW", "Harley Davidson", "Honda", "Yamaha")
  val bmws = List("F650", "K110RS", "R110RT", "R80GS")
  val harleys = List("FXD SuperGlide", "Fat Boy", "Iron 833")
  val hondas = List("CBR600F", "FMX 650", "Gold Wing", "VFR800", "VTR100F")
  val yamahas = List("Fazer 600", "VMax", "YZR-R1", "YZR-R6")

  val parentSelectModel = convertToSelectModel(makes)
  val bwmSelectModel = convertToSelectModel(bmws)
  val harleySelectModel = convertToSelectModel(harleys)
  val hondaSelectModel = convertToSelectModel(hondas)
  val yamahaSelectModel = convertToSelectModel(yamahas)

  val childModelsMap = Map("BMW" -> bwmSelectModel, "Harley Davidson" -> harleySelectModel, "Honda" -> hondaSelectModel,
    "Yamaha" -> yamahaSelectModel)

  private def convertToSelectModel[E](list: List[E]): SelectModel[E] = {
    val options = list.map { value => DefaultOptionModel(value.toString, value) }
    DefaultSelectModel(options)
  }

  private val fromClient: (String, String) => Motorcycle = (makeIndex: String, modelIndex: String) => {
    // Should really check validity of the indexes.
    val make = makes(makeIndex.toInt)
    val model = getBikeSelectModels(makeIndex.toInt).options(modelIndex.toInt)
    Motorcycle(make, model.label)
  }

  private val toClient: Motorcycle => Option[(String, String)] = (bike: Motorcycle) => {
    val makeIndex = if (makes.indexOf(bike.make) > -1) makes.indexOf(bike.make) else 0
    val childSelectModel = childModelsMap(bike.make)
    val modelIndex = childSelectModel.options.indexWhere(opt => opt.label == bike.model)
    if (modelIndex > -1) Some(makeIndex.toString, modelIndex.toString) else Some(modelIndex.toString, "0")
  }

  val motorcycleMapping: Mapping[Motorcycle] =
    mapping("make" -> text, "model" -> text)(fromClient)(toClient)

  val motorcycleForm = Form[Motorcycle](motorcycleMapping)

  def show = Action(
    implicit request => {
      val form = if (flash.get("success").isDefined)
        // Redirect from a submit
        this.motorcycleForm.bind(flash.data)
      else {
        // initial display
        val bike = Motorcycle("BMW", "K110RS")
        this.motorcycleForm.fill(bike)
      }

      Ok(views.html.technique.dynamicSelectAjax(form, parentSelectModel))
    })

  def submit = Action(
    implicit request => {
      // formInput contains the submitted values.
      val formInput: Form[Motorcycle] = this.motorcycleForm.bindFromRequest
      formInput.fold(
        // If the form has validation error, redisplay the page with error.  
        hasErrors = { formWithError =>
          { // In this example, validation always passes.            
            Ok(views.html.technique.dynamicSelectAjax(formWithError, parentSelectModel))
          }
        },

        success = processValid)
    })

  // If there is no validation error, redisplay the entered form data at the bottom of the page.
  private def processValid(motorcycle: Motorcycle)(implicit request: Request[Any]) = {
    val newForm = motorcycleForm.fill(motorcycle);
    val flashDataMap = Flash(newForm.data) + ("success" -> "")
    Redirect(routes.DynamicSelectAjax.show).flashing(flashDataMap)
  }

  def getChildModel = Action(
    implicit request =>
      {
        val makeIndex = extractMakeIndex(request.body.asFormUrlEncoded.get)
        // Should really check makeIndex is within bound.
        val childSelectModels = getBikeSelectModels(makeIndex);
        val bikeOptionModelsJson = childSelectModels.options.zipWithIndex.map {
          case (opt, index) => Json.obj("value" -> index.toString, "label" -> opt.label)
        }

        val json = Json.toJson(bikeOptionModelsJson)
        Ok(json)
      })

      
  private def extractMakeIndex(map: Map[String, Seq[String]]): Int = {
    val makeIndex = map.get("make").getOrElse(List("0")).headOption.getOrElse("0").toInt
    makeIndex
  }

  private def getBikeSelectModels(makeIndex: Int): SelectModel[String] = {
    val make = makes(makeIndex)
    childModelsMap(make)
  }

}