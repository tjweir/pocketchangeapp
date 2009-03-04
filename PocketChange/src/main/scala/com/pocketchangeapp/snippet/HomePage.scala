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


class HomePage {
  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  def summary (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Full(user) => {
      val entries : NodeSeq = user.allAccounts match {
	case Nil => Text("You have no accounts set up") // Add link to create one...
	case accounts => accounts.flatMap({account => 
	  bind("acct", chooseTemplate("account", "entry", xhtml),
	       "name" -> <a href={"/account/" + account.name.is}>{account.name.is}</a>,
	       "balance" -> Text(account.balance.toString))
					 })
      }
      bind("account", xhtml, "entry" -> entries)
    }
    case _ => <lift:embed what="welcome_msg" />
  }
}

