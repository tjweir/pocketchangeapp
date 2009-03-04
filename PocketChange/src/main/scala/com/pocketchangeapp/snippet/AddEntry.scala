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
import com.pocketchangeapp.util.Util

import java.util.Date

//1import net.lag.logging.Logger

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch = {
    case "addentry" => add _
  }

  var account : Long = _
<<<<<<< HEAD:P  var date = ""
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
	  val entryDate = Util.slashDate.parse(date)

	  val amount = BigDecimal(value)

	  // Rework to not throw exceptions
	  val currentAccount = Account.find(account).open_!

	  // We need to determine the last serial number and balance for the date in question
	  val (entrySerial,entryBalance) = Expense.getLastExpenseData(currentAccount, entryDate)

	  println("Last entry = " + (entrySerial, entryBalance))
	  
	  val e = Expense.create.account(account).dateOf(entryDate).serialNumber(entrySerial + 1)
	           .description(desc).amount(BigDecimal(value)).tags(tags)
		   .currentBalance(entryBalance + amount)

	  e.validate match {
            case Nil => {
	      Expense.updateEntries(entrySerial + 1, amount)
              e.save
  	      val acct = Account.find(account).open_!
	      val newBalance = acct.balance.is + e.amount.is
	      acct.balance(newBalance).save
              notice("Entry added!")
	      unregisterThisSnippet() // dpp: remove the statefullness of this snippet
	    }
            case x => error(x)
	  }
	}
      }

        bind("e", in, 
            "account" -> select(user.editable.map(acct => (acct.id.toString, acct.name)), Empty, id => account = id.toLong),
            "dateOf" -> text("", date = _) % ("id" -> "entrydate") % ("maxlength" -> "10") % ("size" -> "10"),
            "desc" -> text("Item Description", desc = _),
            "value" -> text("Value", value = _),
            "tags" -> text(tags, doTagsAndSubmit))
      }
      case _ => Text("")
    }
}

