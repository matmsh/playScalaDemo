package controllers.technique

import java.io.ByteArrayOutputStream
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PiePlot3D
import org.jfree.data.DefaultKeyedValues
import org.jfree.data.general.DefaultPieDataset
import play.api._
import play.api.mvc._
import java.awt.Color
import java.io.ByteArrayInputStream

/**
 * A Demo to render a dynamically generated chart.
 */
object JFreeChartDemo extends Controller {

  def show = Action(
    implicit request =>
      Ok(views.html.technique.jFreeChartDemo()))

  def chart = Action {

    val MimeType = "image/jpeg"
    try {
      val imageData = generateChart

      Ok(imageData).as(MimeType)
    } catch {
      case e: Exception =>
        BadRequest("Couldn’t generate chart. Error: " + e.getMessage)
    }
  }

  private def generateChart():Array[Byte] = {

    val values = new DefaultKeyedValues();
    values.addValue("Firefox", 39)
    values.addValue("Internet Explorer", 20)
    values.addValue("Chrome", 10)
    values.addValue("Safari", 15)
    val pieDataSet = new DefaultPieDataset(values)

    val width = 600
    val height = 400

    val chart = ChartFactory.createPieChart3D(
      "Pie Chart Demo", // chart title	          
      pieDataSet,
      true, // include legend
      false, // tooltips
      false // urls
      );

    val plot: PiePlot3D = chart.getPlot().asInstanceOf[PiePlot3D]
    plot.setForegroundAlpha(0.5f);
    plot.setDepthFactor(0.1);

    val image = chart.createBufferedImage(width, height);
    val byteArray = new ByteArrayOutputStream();
    ChartUtilities.writeBufferedImageAsJPEG(byteArray, image);
    byteArray.toByteArray()
  }

}