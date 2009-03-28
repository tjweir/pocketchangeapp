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
case object NoSuchAccount extends AccountInfo
case object NotPublic extends AccountInfo
case class FullAccountInfo(account : Account, entries : List[Expense]) extends AccountInfo

// The new Loc class that we'll use to control access
object AccountLoc extends Loc[AccountInfo] {
  // Default to "not here"
  def defaultParams = Full(NoSuchAccount)
  
  // We don't want to show this page in the menu
  def params = List(Hidden)
  
  // Required to provide the next three (abstract), but not used
  val link = new Link[AccountInfo](List("account"))
  val name = "Account"
  val text = LinkText[AccountInfo](ignore => Text("Account"))
  
  // Define our page rewriting 
  override def rewrite = Full({
    case RewriteRequest(ParsePath(List("acct", aid), _, _, _), _, _) => {
      Account.findAll(By(Account.stringId, aid)) match {
	case List(account) if account.public.is => {
	  (RewriteResponse("account" :: Nil), 
	   FullAccountInfo(account, account.entries))
	}
	case List(account) => {
	  (RewriteResponse("account" :: Nil), 
	   NotPublic)
	}	  
	case _ => {
	  (RewriteResponse("account" :: Nil),
	   NoSuchAccount)
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
    case ("entries", Full(NoSuchAccount)) => {ignore : NodeSeq => 
      Text("Could not locate the requested account")}
    case ("entries", Full(NotPublic)) => {ignore : NodeSeq => 
      Text("This account is not publicly viewable")}
    case ("entries", Full(FullAccountInfo(account, List()))) => {ignore : NodeSeq =>
      Text("No entries for " + account.name.is)}
    case ("entries", Full(FullAccountInfo(account, entries))) => 
      Accounts.show(entries) _
  }
}
