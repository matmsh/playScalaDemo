package controllers.widgets

import play.api._
import play.api.mvc._
import models.AccessListProducer
import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import models.Access

object DataTablesDemo2 extends Controller {

  def show = Action(
      Ok(views.html.widgets.dataTablesDemo2()))

  def load = Action(
    implicit request => {
      // Search is disabled.
      //val filter = request.getQueryString("sSearch")

      // Might able to use Mapping.
      val pageSize = request.getQueryString("iDisplayLength").get.toInt

      val page = request.getQueryString("iDisplayStart").get.toInt / pageSize
      val order = request.getQueryString("sSortDir_0").get
      val sortCol = request.getQueryString("iSortCol_0").get.toInt
      val sEcho = request.getQueryString("sEcho")

      val asc = (order == "asc")
      val (firstIndex, iTotalRecords, displayRecords) = getList(sortCol, asc, pageSize, page)
      
      val accessListJ = displayRecords.zipWithIndex.map{ case (access,index) =>
        {
          Json.arr(index + firstIndex + 1, access.remoteHost,
               access.accessTime.toString, access.country, access.browser, access.os)
        }}

      val result = Json.obj("sEcho" -> sEcho,
        "iTotalRecords" -> iTotalRecords,
        "iTotalDisplayRecords" -> iTotalRecords,
        "aaData" -> accessListJ)

      val json = Json.toJson(result)
      Ok(json)
    })

  /**
   *  sortCol - index of the column to be sorted. Eg sortCol = 1 means sort on remoteHost.
   *  asc - true if sort ascendingly.
   *  pageSize : No of records per page
   *  page : (O based). The page to be returned.
   *
   *  Return (indexOfFirst display record, total no of records, records to be displayed)
   */
  private def getList(sortCol: Int, asc: Boolean, pageSize: Int, page: Int): (Int, Int, List[Access]) = {

    val accessListSorted = getSortedList(sortCol, asc)
    val indexOfLast = accessListSorted.size - 1
    val firstIndex = page * pageSize
    val lastIndex = Math.min(firstIndex + pageSize - 1, indexOfLast)

    val resultList = (firstIndex to lastIndex).map(i => accessListSorted(i)).toList
    (firstIndex, accessListSorted.size, resultList)
  }

  private def getSortedList(sortCol: Int, asc: Boolean): List[Access] = {
    val accessList = AccessListProducer.getList

    // Col 0 is the "Row no", which has sort disabled.
    if (sortCol == 0) {
      accessList
    } else if (asc) {
      sortCol match {
        case 1 => accessList.sortBy(access => access.remoteHost)
        case 2 => accessList.sortBy(access => access.accessTime)
        case 3 => accessList.sortBy(access => access.country)
        case 4 => accessList.sortBy(access => access.browser)
        case 5 => accessList.sortBy(access => access.os)
      }
    } else {
      sortCol match {
        case 1 => accessList.sortWith(_.remoteHost > _.remoteHost)
        case 2 => accessList.sortWith((x, y) => x.accessTime.getTime() > y.accessTime.getTime())
        case 3 => accessList.sortWith(_.country > _.country)
        case 4 => accessList.sortWith(_.browser > _.browser)
        case 5 => accessList.sortWith(_.os > _.os)

      }

    }
  }

}