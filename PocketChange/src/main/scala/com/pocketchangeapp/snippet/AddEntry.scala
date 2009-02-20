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

import java.util.Date

import net.lag.logging.Logger

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch = {
    case "addentry" => add _
  }

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  var account : Long = _
  var date = ""
  var desc = ""
  var value = ""
  var tags = S.param("tag") openOr ""
  
  def add(in: NodeSeq): NodeSeq = User.currentUser match {
    case Full(user) if user.editable.size > 0 => {

      def doTagsAndSubmit(t: String) {
	tags = t
	if (tags.trim.length == 0) error("We're going to need at least one tag.")
	else {
          /* Get the date correctly, add the datepicker: comes in as yyyy/mm/dd */
	  val e = Transaction.create.account(account).dateOf(formatter.parse(date)).summary(desc).amount(BigDecimal(value)).tags(tags)
	  e.validate match {
            case Nil => {
              e.save
  	      val acct = Account.find(account).open_!
	      Logger.get.info("Current balance = " + acct.balance.is)
	      val newBalance = acct.balance.is + e.amount.is
	      Logger.get.info("New balance = " + newBalance)
	      acct.balance(newBalance)
	      Logger.get.info("is = " + acct.balance.is + ", was = " + acct.balance.was)
	      val saved = acct.save
	      Logger.get.info("Saved = " + saved)
              notice("Entry added!")
	      unregisterThisSnippet() // dpp: remove the statefullness of this snippet
	    }
            case x => error(x)
	  }
	}
      }

      bind("e", in, 
	   "account" -> select(user.editable.map(acct => (acct.id.toString, acct.name)), Empty, id => account = id.toLong),
	   "dateOf" -> text(formatter.format(new Date()).toString, date = _) % ("id" -> "entrydate"),
	   "desc" -> text("Item Description", desc = _),
	   "value" -> text("Value", value = _),
	   "tags" -> text(tags, doTagsAndSubmit))
    }
    case _ => Text("")
  }
}
 
