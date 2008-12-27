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

import java.math.BigDecimal
import java.util.Date

import net.lag.logging.Logger

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch = {
    case "addentry" => add _
  }

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  var date = ""
  var desc = ""
  var value = ""
  var tags = S.param("tag") openOr ""
  
  def add(in: NodeSeq): NodeSeq = {
    def doTagsAndSubmit(t: String) {
      tags = t
      if (tags.trim.length == 0) error("We're going to need at least one tag.")
      else {
        /* Get the date correctly, add the datepicker: comes in as yyyy/mm/dd */
      val e = (new Entry).dateOf(formatter.parse(date)).description(desc).amount(new BigDecimal(value)).tags(tags)
      e.validate match {
        case Nil => 
          e.save
          notice("Entry added!")
          unregisterThisSnippet() // dpp: remove the statefullness of this snippet
        case x => error(x)
      }
      }
    }
    
    bind("e", in, 
      "dateOf" --> text(formatter.format(new Date()).toString, date = _) % ("id" -> "entrydate"),
      "desc" --> text("Item Description", desc = _),
      "value" --> text("Value", value = _),
      "tags" --> text(tags, doTagsAndSubmit))
  }
}
 
