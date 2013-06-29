package controllers.technique

import play.api.mvc._
import play.api._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json._
import play.api.templates.Html

object DynamicSelect extends Controller {

  case class Motorcycle(make: String, model: String)
  case class SelectOpt(value: String, label: String)

  val makes = List("BMW", "Harley Davidson", "Honda", "Yamaha")
  val bmws = List("F650", "K110RS", "R110RT", "R80GS")
  val harleys = List("FXD SuperGlide", "Fat Boy", "Iron 833")
  val hondas = List("CBR600F", "FMX 650", "Gold Wing", "VFR800", "VTR100F")
  val yamahas = List("Fazer 600", "VMax", "YZR-R1", "YZR-R6")

  val parentSelectModel = convertToSelectOpts(makes)
  val bwmModels = convertToSelectOpts(bmws)
  val harleyModels = convertToSelectOpts(harleys)
  val hondaModels = convertToSelectOpts(hondas)
  val yamahaModels = convertToSelectOpts(yamahas)

  val childModelsMap = Map("BMW" -> bwmModels, "Harley Davidson" -> harleyModels, "Honda" -> hondaModels,
    "Yamaha" -> yamahaModels)

  val childModelsMapJson: String = buildJson(childModelsMap)

  private val fromClient= (makeIndex:String, modelIndex:String) => {
        // Should really check validity of the indexes.
        val make = makes(makeIndex.toInt)
        val model = childModelsMap(make)(modelIndex.toInt)
        Motorcycle(make, model.label)
  }
  
  private val toClient = (bike:Motorcycle) => {
       val makeIndex =  if ( makes.indexOf(bike.make) > -1) makes.indexOf(bike.make) else 0
       val childModel = childModelsMap(bike.make)
       val modelIndex = childModel.indexWhere( opt => opt.label == bike.model)
       if (modelIndex > -1) Some(makeIndex.toString, modelIndex.toString) else Some(modelIndex.toString,"0")
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

      Ok(views.html.technique.dynamicSelect(form, parentSelectModel,  childModelsMapJson))
    })

  def submit = Action(
    implicit request => {
      // formInput contains the submitted values.
      val formInput: Form[Motorcycle] = this.motorcycleForm.bindFromRequest
      formInput.fold(
        // If the form has validation error, redisplay the page with error.  
        hasErrors = { formWithError =>
          { // In this example, validation always passes.            
            Ok(views.html.technique.dynamicSelect(formWithError, parentSelectModel,
               childModelsMapJson))
          }
        },

        success = processValid)
    })

  // If there is no validation error, redisplay the entered form data at the bottom of the page.
  private def processValid(motorcycle: Motorcycle)(implicit request: Request[Any]) = {
    val newForm = motorcycleForm.fill(motorcycle);
    val flashDataMap = Flash(newForm.data) + ("success" -> "")
    Redirect(routes.DynamicSelect.show).flashing(flashDataMap)
  }

  private def buildJson(dataMap: Map[String, List[SelectOpt]]): String = {
    val jsonArray = dataMap.map { case (make, models) => buildMakeJson(make, models) }.mkString("[", ",", "]")
    jsonArray
  }

  private def buildMakeJson(make: String, models: List[SelectOpt]): String = {
    val modelsJson = models.map(opt => Json.obj("value" -> opt.value, "label" -> opt.label))
    val json = Json.obj("parent" -> makes.indexOf(make).toString, "models" -> modelsJson)
    Json.prettyPrint(json)
  }

  private def convertToSelectOpts(list: List[String]) = {
    list.zipWithIndex.map { case (str, index) => SelectOpt(index.toString, str) }
  }
}