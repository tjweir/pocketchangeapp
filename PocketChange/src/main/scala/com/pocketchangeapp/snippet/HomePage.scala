package com.pocketchangeapp {
package snippet {

import java.util.Date

import scala.xml.{NodeSeq,Text}

import net.liftweb.common.Full
import net.liftweb.util.Helpers.{bind,chooseTemplate}

import model.User

// Import Helper's implicits for binding
import net.liftweb.util.Helpers._

class HomePage {
  def summary (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Full(user) => {
      val entries : NodeSeq = user.allAccounts match {
	case Nil => Text("You have no accounts set up") // TODO: Add link to create one...
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

// Close package statements
}}
