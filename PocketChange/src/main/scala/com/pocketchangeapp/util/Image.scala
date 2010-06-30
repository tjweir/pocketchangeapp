/*
 * The split package here allows us to reference top-level
 * PocketChange packages, like model, snippet, etc.
 */
package com.pocketchangeapp {
package util {

import net.liftweb.common.Full
import net.liftweb.http.{ForbiddenResponse,InMemoryResponse,LiftResponse,PlainTextResponse,RedirectResponse}

import model.{Expense,User}

object Image {
  def viewImage(id : String) : LiftResponse = 
    try {
      User.currentUser match {
	case Full(user) => {
	  Expense.find(id.toLong) match {
	    case Full(e) => {
	      if (e.owner == User.currentUser) {
		InMemoryResponse(e.receipt.is, List("Content-Type" -> e.receiptMime.is), Nil, 200)
	      } else {
		ForbiddenResponse()
	      }
	    }
	    case _ => PlainTextResponse("No such expense item", Nil, 404)
	  }
	}
	case _ => RedirectResponse("/user_mgt/login")
      }
    } catch {
      case nfe : NumberFormatException => PlainTextResponse("Invalid expense ID", Nil, 400)
    }
}

// Close package statements
}}
