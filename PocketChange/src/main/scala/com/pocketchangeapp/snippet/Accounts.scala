package com.pocketchangeapp.snippet

import java.util.Date

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

class Accounts {

  def manage (xhtml : NodeSeq) : NodeSeq = {
    def deleteAccount (acct : Account) {
      // No cascade???
      acct.admins.foreach(_.delete_!)
      acct.viewers.foreach(_.delete_!)
      acct.transactions.foreach({
	tx =>
	  tx.tags.foreach(_.delete_!) // Need to delete tx tags, too
	  tx.delete_!
      })
      acct.tags.foreach(_.delete_!)
      acct.notes.foreach(_.delete_!)
      acct.delete_!
    }

    User.currentUser.map({user =>
      user.accounts.flatMap({acct => 
	bind("acct", chooseTemplate("account", "entry", xhtml),
	     "name" -> Text(acct.name.is),
	     "description" -> Text(acct.description.is),
	     "actions" -> { link("/manage", () => acct.delete_!, Text("Delete")) ++ Text(" ") ++
			    link("/editAcct", () => currentAccountVar(acct), Text("Edit")) })
			   })
			}) openOr Text("You're not logged in")
  }

  object currentAccountVar extends RequestVar[Account]({
    Account.create.owner(User.currentUser.open_!)
  })
  def currentAccount = currentAccountVar.is

  def edit (xhtml : NodeSeq) : NodeSeq = {
    def doSave () = {
      
      currentAccount.validate match {
	case Nil => 
	  currentAccount.save
	redirectTo("/manage")
	case x => error(x)
      }
    }
    
    val acct = currentAccount

    bind("acct", xhtml,
	 "id" -> hidden(() => currentAccountVar(acct)),
	 "name" -> text(currentAccount.name.is, currentAccount.name(_)),
	 "description" -> text(currentAccount.description.is, currentAccount.description(_)),
	 "save" -> submit("Save", doSave))
  }

  def detail (xhtml: NodeSeq) : NodeSeq = S.param("name") match {
    case Full(acctName) => {
      Account.findByName(User.currentUser.open_!, acctName) match {
	case acct :: Nil => {
	  val tags = <a href={"/viewAcct/" + acct.name.is}>All tags</a> ++ Text(" ") ++ 
	         acct.tags.flatMap({tag => <a href={"/viewAcct/" + acct.name.is + "/" + tag.tag.is}>{tag.tag.is}</a> ++ Text(" ")})

	  // Some state for the Ajax calls
	  var startDate : Box[Date] = Empty
	  var endDate : Box[Date] = Empty

	  val tableEntries = (S.param("tag") match {
	    case Full(tag) => acct.transactions.filter(_.tags.exists(_.tag == tag)) // Can probably be made more efficient
	    case _ => acct.transactions
	  })

	  bind("acct", xhtml, 
	       "name" -> acct.name.asHtml,
	       "balance" -> acct.balance.asHtml,
	       "tags" -> tags,
	       "startDate" -> Text(""),
	       "endDate" -> Text(""),
	       "graph" -> <img src={"/graph/" + acctName + "/history"} />,
	       "table" -> buildTxTable(tableEntries, xhtml))
	}
	case _ => Text("Could not locate account " + acctName)
      }
    }
    case _ => Text("No account name provided")
  }

  def buildTxTable(entries : List[Transaction], template : NodeSeq) = {
    entries.flatMap({ entry =>
      bind("entry", chooseTemplate("acct", "tableEntry", template),
	   "date" -> Text(Util.slashDate.format(entry.dateOf.is)),
	   "desc" -> Text(entry.description.is),
	   "tags" -> Text(entry.tags.map(_.tag.is).mkString(", ")),
	   "amt" -> Text(entry.amount.toString),
	   "balance" -> Text(entry.currentBalance.toString))
		    })
  }
}
