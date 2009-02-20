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

  def summary (xhtml: NodeSeq) : NodeSeq = S.param("name") match {
    case Full(acctName) => {
      Account.findByName(User.currentUser.open_!, acctName) match {
	case acct :: Nil => {
	  // Set this so that the transactionList page can see it.
	  currentAccountVar(acct)
	  val tags = <a href={"/viewAcct/" + acct.name.is}>All tags</a> ++ Text(" ") ++ 
	         acct.tags.flatMap({tag => <a href={"/viewAcct/" + acct.name.is + "/" + tag.tag.is}>{tag.tag.is}</a> ++ Text(" ")})
	  bind("acct", xhtml, 
	       "name" -> acct.name.asHtml,
	       "balance" -> acct.balance.asHtml,
	       "tags" -> tags)
	}
	case _ => Text("Could not locate account " + acctName)
      }
    }
    case _ => Text("No account name provided")
  }

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  def transactionList (xhtml: NodeSeq) : NodeSeq = {
    val entries : NodeSeq = (S.param("tag") match {
      case Full(tag) => currentAccount.transactions.filter(_.tags.exists(_.tag == tag)) // Can probably be made more efficient
      case _ => currentAccount.transactions
    }).flatMap({ entry =>
      bind("entry", chooseTemplate("acct", "entry", xhtml),
	   "date" -> Text(formatter.format(entry.dateOf.is)),
	   "desc" -> Text(entry.summary.is),
	   "tags" -> Text(entry.tags.map(_.tag.is).mkString(", ")),
	   "amt" -> Text(entry.amount.toString))
	     })

    bind("acct", xhtml, "entry" -> entries)
  }
}
