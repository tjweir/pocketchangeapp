package com.pocketchangeapp.snippet

import scala.xml._
import net.liftweb.http._
import net.liftweb.util._
import S._
import SHtml._
import Helpers._
import scala.xml._

import com.pocketchangeapp.model._

import java.util.Date

import net.lag.logging.Logger

class HomePage {
  val log = Logger.get

  def howdy = <span>Welcome to PocketChange at {new java.util.Date}</span>

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  def summary (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Full(user) => {
      val entries : NodeSeq = user.allAccounts match {
	case Nil => Text("You have no accounts set up") // Add link to create one...
	case accounts => accounts.flatMap({account => 
	  bind("acct", chooseTemplate("account", "entry", xhtml),
	       "name" -> <a href={"/viewAcct/" + account.name.is}>{account.name.is}</a>,
	       "balance" -> Text(account.balance.toString))
					 })
      }
      bind("account", xhtml, "entry" -> entries)
    }
    case _ => Text("")
  }

  def welcome (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Empty => <lift:embed what="welcome_msg" />
    case _ => Text("")
  }

}

