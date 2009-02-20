package com.pocketchangeapp.snippet

import scala.xml._
import net.liftweb._
import http._
import util._
import S._
import SHtml._
import scala.xml._
import Helpers._

import com.pocketchangeapp.model._

class Util {
  def splitEvery[A](as : List[A], n : Int) : List[List[A]] = as.splitAt(n) match {
    case (a, Nil) => a :: Nil
    case (a, b)   => a :: splitEvery(b, n)
  }
}
