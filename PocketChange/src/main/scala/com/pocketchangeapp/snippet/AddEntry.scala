package com.pocketchangeapp
package snippet

import scala.xml.{NodeSeq,Text}

import net.liftweb.common.{Box,Empty,Full,Logger}
import net.liftweb.http.{FileParamHolder,S,SHtml,StatefulSnippet}
import org.slf4j.LoggerFactory
import java.text.ParseException

// Import "bind", as well as the implicits that make bind easy
import net.liftweb.util.Helpers._

import model.{Account,Expense,User}
import util.Util

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet with Logger {
  // Use a different name for our logger
  override val _logger = LoggerFactory.getLogger("EntryEdit")

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

        // Pre-validate the unparsed values
        val requiredFields = List((date, "a date"), (desc, "a description"), (value, "a value"), (tags, "tags"))

        val missing = requiredFields.flatMap { case (field,label) =>
          if (field.trim.length == 0) {
            Some(label)
          } else {
            None
          }
        }

        if (! missing.isEmpty) {
          missing.foreach { label =>
            error("Entry add attempted without " + label)
            S.error("You must provide " + label)
          }
        } else {
          try {
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
                error("Received invalid file attachment: " + something)
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
                S.redirectTo("/")
              }
              case (x,_) => {
                error(x)
                S.error(x)
              }
            }
          } catch {
            case pe : ParseException => error(pe); S.error("Invalid date: " + date)
            case nfe : NumberFormatException => error(nfe); S.error("Invalid value: " + value)
          }
	      }
      }

      bind("e", in, 
           "account" -%> SHtml.select(user.editable.map(acct => (acct.id.toString, acct.name.toString)), Empty, id => account = id.toLong),
           // Note that we use "-%>" so that the id and maxlength attrs on the template are preserved
           "dateOf" -%> SHtml.text(date, date = _),
           "desc" -%> SHtml.text(desc, desc = _),
           "value" -%> SHtml.text(value, value = _),
	         "receipt" -%> SHtml.fileUpload(fph => fileHolder = Full(fph)),
           "tags" -%> SHtml.text(tags, doTagsAndSubmit))
    }
    case _ => Text("")
  }
}
