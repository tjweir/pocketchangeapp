package com.pocketchangeapp.util

import java.util.Date
import java.text.SimpleDateFormat

import scala.xml._
import net.liftweb._
import http._
import util._
import S._
import SHtml._
import scala.xml._
import Helpers._

import com.pocketchangeapp.model._

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

  def getDateParam(name : String, converter : (String) => Date) : Box[Date] =
    try {
      S.param(name).map(converter(_))
    } catch {
      case e => Empty
    }  
}
