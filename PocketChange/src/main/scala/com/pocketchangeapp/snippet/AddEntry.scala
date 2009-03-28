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

/* date | desc | tags | value */ 
class AddEntry extends StatefulSnippet {
  def dispatch = {
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
      def doAdd (t : String) = {
	tags = t
	if (Accounts.addEntry(account,value,date,tags,desc,fileHolder)) {
	  println("Add OK")
	  unregisterThisSnippet() // remove the statefullness of this snippet
	} else {
	  println("Add failed")
	}
      }

      bind("e", in, 
           "account" -> 
	   select(user.editable.map(acct => (acct.id.toString, acct.name)), Empty, id => account = id.toLong),
           "dateOf" -> text("", date = _) % ("id" -> "entrydate") % ("maxlength" -> "10") % ("size" -> "10"),
           "desc" -> text("", desc = _),
           "value" -> text("", value = _),
	   "receipt" -> fileUpload(fph => fileHolder = Full(fph)),
           "tags" -> text(tags, doAdd))
    }
    case _ => Text("")
  }
}

