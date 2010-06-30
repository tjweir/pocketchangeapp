package com.pocketchangeapp {
package snippet {

import scala.xml.{NodeSeq,Text}

import java.util.Date

import net.liftweb.common.{Box,Empty,Full,Logger}
import net.liftweb.http.{FileParamHolder,S,SHtml,StatefulSnippet}

// Import "bind", as well as the implicits that make bind easy
import net.liftweb.util.Helpers._

import model.{Account,Expense,User}
import util.Util

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch : DispatchIt = {
    case "addentry" => add _
  }

  var account : Long = _
  var date = ""
  var desc = ""
  var value = ""
  var tags = S.param("tag") openOr ""
  var fileHolder : Box[FileParamHolder] = Empty
  
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

	  val e = 
            Expense.create.account(account).dateOf(entryDate).serialNumber(entrySerial + 1)
	      .description(desc).amount(BigDecimal(value)).tags(tags)
	      .currentBalance(entryBalance + amount)

	  // Add the optional receipt if it's the correct type
	  val receiptOk = fileHolder match {
	    case Full(FileParamHolder(_, null, _, _)) => true
	    case Full(FileParamHolder(_, mime, _, data)) 
	    if mime.startsWith("image/") => {
	      e.receipt(data).receiptMime(mime)
	      true
	    }
	    // If someone sends nothing...
	    case Full(FileParamHolder(_, _, "", _)) => true
	    case Full(something) => {
	      Logger(classOf[AddEntry]).error("Received invalid file attachment: " + something)
	      S.error("Invalid receipt attachment")
	      false
	    }
	    case _ => true
	  }
	  
	  (e.validate,receiptOk) match {
            case (Nil,true) => {
	      Expense.updateEntries(entrySerial + 1, amount)
              e.save
  	      val acct = Account.find(account).open_!
	      val newBalance = acct.balance.is + e.amount.is
	      acct.balance(newBalance).save
              S.notice("Entry added!")
	      this.unregisterThisSnippet() // dpp: remove the statefullness of this snippet
	    }
            case (x,_) => S.error(x)
	  }
	}
      }

      bind("e", in, 
           "account" -> SHtml.select(user.editable.map(acct => (acct.id.toString, acct.name)), Empty, id => account = id.toLong),
           // Note that we use "-%>" so that the id and maxlength attrs on the template are preserved
           "dateOf" -%> SHtml.text("", date = _) % ("size" -> "10"),
           "desc" -> SHtml.text("", desc = _),
           "value" -> SHtml.text("", value = _),
	   "receipt" -> SHtml.fileUpload(fph => fileHolder = Full(fph)),
           "tags" -> SHtml.text(tags, doTagsAndSubmit))
    }
    case _ => Text("")
  }
}

// Close package statements
}}
