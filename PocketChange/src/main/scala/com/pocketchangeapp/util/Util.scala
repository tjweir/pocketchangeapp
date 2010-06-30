package com.pocketchangeapp.util

import java.util.Date
import java.text.SimpleDateFormat

import net.liftweb.common.{Box,Empty,Full}
import net.liftweb.http.S

object Util {
  val noSlashDate = new SimpleDateFormat("yyyyMMdd")

  val slashDate = new SimpleDateFormat("yyyy/MM/dd")

  def splitEvery[A](as : List[A], n : Int) : List[List[A]] = as.splitAt(n) match {
    case (a, Nil) => a :: Nil
    case (a, b)   => a :: splitEvery(b, n)
  }

  def getIntParam(name : String, default : Int) : Int = {
    try { 
      S.param(name).map(_.toInt) openOr default
    }
    catch { 
      case e => default // Should log something in this case
    } 
  }

  type DateConverter = String => Date

  def parseDate(value : String, converter : DateConverter) : Box[Date] =
    try {
      Full(converter(value))
    } catch {
      case e => Empty
    }  

  def getDateParam(name : String, converter : DateConverter) : Box[Date] = {
    S.param(name).map(parseDate(_, converter)) openOr Empty
  }
}
