package com.pocketchangeapp
package snippet

import scala.xml.{NodeSeq,Text}

import model.User
import net.liftweb.common.{Logger, Full}
import net.liftweb.http.DispatchSnippet

// Import Helper's implicits for binding
import net.liftweb.util.Helpers._

/**
 * This is the home page snippet. Unlike the Accounts and AddEntry snippet classes,
 * this snippet class is resolved via reflection. Because we only expose a single render
 * method this doesn't put us at risk of divulging private methods.
 */
class HomePage extends Logger {
  def render (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
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
