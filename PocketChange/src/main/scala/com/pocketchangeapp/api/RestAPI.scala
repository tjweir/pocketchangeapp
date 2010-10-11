/*
 * RestAPI.scala
 *
 * Copyright 2008-2010 Derek Chen-Becker, Marius Danciu and Tyler Wier
 * 
 */
package com.pocketchangeapp {
package api {

import java.text.SimpleDateFormat

import scala.xml.{Elem, Node, NodeSeq, Text}

import net.liftweb.common.{Box,Empty,Failure,Full,Logger}
import net.liftweb.http.{AtomResponse,BadResponse,CreatedResponse,
                         GetRequest,JsonResponse,LiftResponse,LiftRules,
                         NotFoundResponse,ParsePath,PutRequest,Req,
                         ResponseWithReason,RewriteRequest}
import net.liftweb.http.rest.XMLApiHelper
import net.liftweb.mapper.By

import model._

object DispatchRestAPI extends XMLApiHelper {
  final val logger = Logger("com.pocketchangeapp.api.DispatchRestAPI")

  // Import our methods for converting things around
  import RestFormatters._

  /**
   * This method provides the dispatch hooks for our REST API that
   * we'll hook into LiftRules.dispatch
   */
  def dispatch: LiftRules.DispatchPF = {     
    // Define our getters first
    case Req(List("api", "expense", Expense(expense,_)), _, GetRequest) => 
      () => nodeSeqToResponse(toXML(expense)) // default to XML
    case Req(List("api", "expense", Expense(expense,_), "xml"), _, GetRequest) => 
      () => nodeSeqToResponse(toXML(expense))
    case Req(List("api", "expense", Expense(expense,_), "json"), _, GetRequest) => 
      () => JsonResponse(toJSONExp(expense), Nil, Nil, 200)
    case Req(List("api", "account", Account(account)), _, GetRequest) =>
      () => AtomResponse(toAtom(account))

    // Define the PUT handler for both XML and JSON MIME types
    case request @ Req(List("api", "account", Account(account)), _, PutRequest) 
      if request.xml_? => 
        () => addExpense(fromXML(request.xml,account), 
                         account, 
                         result => CreatedResponse(toXML(result), "text/xml"))
    case request @ Req(List("api", "account", Account(account)), _, PutRequest) 
      if request.json_? => 
        () => addExpense(fromJSON(request.body,account), 
                         account,
                         result => JsonResponse(toJSONExp(result), Nil, Nil, 201))

    // Invalid API request - route to our error handler
    case Req("api" :: x :: Nil, "", _) => 
      () => BadResponse() // Everything else fails
  }

  /*
   * This partial function defines how we protect our API
   * using the HttpAuthentication functionality in Lift. This
   * will hook into LiftRules.httpAuthProtectedResource.
   */
  import net.liftweb.http.auth.AuthRole
  def protection : LiftRules.HttpAuthProtectedResourcePF = {
    case Req(List("api", "account", accountId), _, PutRequest) => 
      Full(AuthRole("editAcct:" + accountId))
    case Req(List("api", "account", accountId), _, GetRequest) => 
      Full(AuthRole("viewAcct:" + accountId))
    // If the account is public, don't enforce auth
    case Req(List("api", "expense", Expense(e, true)), _, GetRequest) => Empty
    case Req(List("api", "expense", Expense(e, _)), _, GetRequest) =>
      Full(AuthRole("viewAcct:" + e.account.obj.open_!.id))
  }

  def createTag (xml : NodeSeq) : Elem = <pca_api>{xml}</pca_api>

  // reacts to the PUT Request
  def addExpense(parsedExpense : Box[Expense], 
                 account : Account, 
                 success : Expense => LiftResponse): LiftResponse = parsedExpense match {
    case Full(expense) => {
      val (entrySerial,entryBalance) = 
        Expense.getLastExpenseData(account, expense.dateOf)

      expense.account(account).serialNumber(entrySerial + 1).
      currentBalance(entryBalance + expense.amount)
        
      expense.validate match {
        case Nil => {
          Expense.updateEntries(entrySerial + 1, expense.amount.is)
          expense.save
          
          account.balance(account.balance.is + expense.amount.is).save

          success(expense)
        }
        case errors => {
          val message = errors.mkString("Validation failed:", ",","")
          logger.error(message)
          ResponseWithReason(BadResponse(), message)
        }
      }
    }
    case Failure(msg, _, _) => {
      logger.error(msg)
      ResponseWithReason(BadResponse(), msg)
    }
    case error => {
      logger.error("Parsed expense as : " + error)
      BadResponse()
    }
  }
}


// Close package statements
}}
