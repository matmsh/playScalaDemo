package models

import java.util.Date
import play.api.Play.current
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import play.api.Play

case class Access(remoteHost: String, country: String, accessTime: Date, browser: String, os: String)

object AccessListProducer {

  /**
   * Read the csv file conf/accessLog.txt and produce a list of Access objects.
   */
  def getList(): List[Access] = {
    // Read file conf/accessLog.txt
    val inputStream = Play.classloader.getResourceAsStream("accessLog.txt")
    val lines = io.Source.fromInputStream(inputStream).getLines
    lines.map(convert(_)).toList

  }

  def convert(line: String): Access = {
    val dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.sss");
    val tokens = line.split(',')
    val remoteHost = tokens(0)
    val country = tokens(1)
    val accessTime = dateFormatter.parse(tokens(2))
    val browser = tokens(3)
    val os = tokens(4)
    Access(remoteHost, country, accessTime, browser, os)

  }

}