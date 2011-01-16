package com.pocketchangeapp
package snippet

import scala.xml.{NodeSeq,Text}

import model.User
import net.liftweb.common.{Logger, Full}

// Import Helper's implicits for binding
import net.liftweb.util.BindHelpers._

/**
 * This is the home page snippet. Unlike the Accounts and AddEntry snippet classes,
 * this snippet class is resolved via reflection. Because we only expose a single render
 * method this doesn't put us at risk of divulging private methods.
 */
class HomePage extends Logger {
  def render (xhtml : NodeSeq) : NodeSeq = User.currentUser match {
    case Full(user) => {
      user.allAccounts match {
	      case Nil => Text("You have no accounts set up, ") ++
          <lift:Menu.item name="Add Account">please add one</lift:Menu.item>
	      case accounts =>
          ("#summary" #> {
              "li" #> accounts.map { account =>
                "a [href]" #> ("/account/" + account.name.is) &
                "a *" #> account.name &
                "a [class+]" #> "foo" &
                "#balance" #> account.balance
              }
           } &
           "#total" #> accounts.map(_.balance.is).sum.toString
          ).apply(xhtml) // apply the CSS binding to the input XHTML
			}
    }
    case _ => <lift:embed what="welcome_msg" />
  }
}
