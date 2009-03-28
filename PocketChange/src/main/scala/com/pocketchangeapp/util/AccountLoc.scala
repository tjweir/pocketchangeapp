package com.pocketchangeapp.util

import _root_.scala.xml.{NodeSeq,Text}

import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.util._
import Helpers._

import _root_.net.liftweb.sitemap._
import Loc._
import _root_.net.liftweb.mapper._

import _root_.com.pocketchangeapp.model._
import _root_.com.pocketchangeapp.snippet.Accounts

// Define the type-safe case classes for the Loc to use
abstract class AccountInfo
case object NoSuchAccountInfo extends AccountInfo
case class FullAccountInfo(account : Account, entries : List[Expense]) extends AccountInfo

// The new Loc class that we'll use to control access
object AccountLoc extends Loc[AccountInfo] {
  // Default impls
  def defaultParams = Empty
  def params = List(Hidden)
  val link = new Link[AccountInfo](List("account"))
  val name = "Account"
  val text = LinkText[AccountInfo](ignore => Text("Account"))
  
  // Define our page rewriting 
  override def rewrite = Full({
    case RewriteRequest(ParsePath(List("acct", aid), _, _, _), _, _) => {
      Account.findAll(By(Account.stringId, aid)) match {
	case List(account) => {
	  (RewriteResponse("account" :: Nil), 
	   FullAccountInfo(account, account.entries))
	}
	case _ => {
	  (RewriteResponse("account" :: Nil),
	   NoSuchAccountInfo)
	}
      }
    }
  })

  // Define the title based on the param
  override def title(param : AccountInfo) = param match {
    case FullAccountInfo(acct, _) => Text("Expense summary for " + acct.name.is)
    case _ => Text("No account")
  }

  // Define snippet behavior based on the param
  override def snippets = {
    case ("entries", Full(NoSuchAccountInfo)) => {ignore : NodeSeq => 
      Text("Could not locate the requested account")}
    case ("entries", Full(FullAccountInfo(account, List()))) => {ignore : NodeSeq =>
	Text("No entries for " + account.name.is)}
    case ("entries", Full(FullAccountInfo(account, entries))) => 
      Accounts.show(entries) _
  }
}
