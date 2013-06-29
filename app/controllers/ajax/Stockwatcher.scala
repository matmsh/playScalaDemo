package controllers.ajax

import play.api.mvc._
import play.api._
import play.api.libs.json._
import play.api.data._
import play.api.data.Forms._
import java.text.DecimalFormat
import java.text.SimpleDateFormat
import java.util.GregorianCalendar
import java.util.Calendar


object Stockwatcher extends Controller {
  val changeFormatter = new DecimalFormat("+#,##0.00;-#,##0.00");
  val priceFormatter = new DecimalFormat("#,##0.00");
  val timeFormatter = new SimpleDateFormat("yyyy MMM dd hh:mm:ss");  
     
  case class StockPrice(symbol:String,price:Double,change:Double, changeInPct:Double)


  def show = Action(
    implicit request =>
      Ok(views.html.ajax.stockwatcher())
   )

  def getPrices = Action(
    implicit request =>
      {
        val symbols = request.body.asFormUrlEncoded.map(m => extractSymbols(m)).getOrElse(Nil)
        val stockPrices = refreshPrices(symbols);
        val result = stockPrices.map(stockPriceToJson(_))

        val currentTime = timeFormatter.format((new GregorianCalendar()).getTime())
        val currentTimeJson = Json.obj("currentTime" -> currentTime)

        val json = Json.toJson(currentTimeJson :: result)

        Ok(json)
      })

      
  /**
   *   Extract symbols from the map.   
   */    
  private def extractSymbols(map: Map[String, Seq[String]]): List[String] = {
    val symbols = map.filter { case (key, value) => key.startsWith("symbols[") }.map 
                   { case (key, value) => value.headOption.getOrElse("") }.toList
    symbols.distinct.filter( _.size >0 ).sorted
  }  
      
  
  /**
   * Return a list of prices for a given list of symbols
   */
  private def refreshPrices(symbols:List[String]):List[StockPrice]={
    val MAX_PRICE = 100.0; // $100.00  
    val MAX_PRICE_CHANGE = 0.02; // +/- 2%  
   
    symbols.map( symbol => { val price=  Math.random() * MAX_PRICE;
                             val change =  price * MAX_PRICE_CHANGE * (Math.random() * 2.0 - 1.0);  
                             val changeInPct = 100.0 * change/price;
                             StockPrice(symbol,price,change,changeInPct)
    })
        
  }
  
  private def stockPriceToJson(stockPrice:StockPrice)={
     Json.obj("symbol" -> stockPrice.symbol, "price" -> priceFormatter.format(stockPrice.price),
                       "change" -> changeFormatter.format( stockPrice.change),
                       "changeInPct" -> changeFormatter.format(stockPrice.changeInPct))
  }
  
}