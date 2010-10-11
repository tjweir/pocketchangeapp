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
                         ResponseWithReason,RewriteRequest, XmlResponse}
import net.liftweb.http.rest.{RestHelper,XMLApiHelper}
import net.liftweb.http.js.JE.JsRaw
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
      () => JsonResponse(toJSON(expense))
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
                         result => JsonResponse(toJSON(result), Nil, Nil, 201))

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

  // We're overriding this because we don't want the operation and success tags
  override def buildResponse (success: Boolean, 
                              msg : Box[NodeSeq],
                              body : NodeSeq) = XmlResponse(body.first)

  def createTag(in : NodeSeq) : Elem = <unused/>

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

object RestHelperAPI extends RestHelper {
  // Import our methods for converting things around
  import RestFormatters._

  // Service Atom and requests that don't request a specific format
  serve {
    // Default to XML
    case Get(List("api", "expense", Expense(expense,_)), _) => 
      () => Full(toXML(expense))
    case Get(List("api", "account", Account(account)), _) =>
      () => Full(AtomResponse(toAtom(account)))
  }

  // Hook our PUT handlers
  import DispatchRestAPI.addExpense
  serve {
    case XmlPut(List("api", "account", Account(account)), (body, request)) =>
      () => Full(addExpense(fromXML(Full(body),account),
                            account, 
                            result => CreatedResponse(toXML(result), "text/xml")))
    case JsonPut(List("api", "account", Account(account)), (_, request))  =>
      () => Full(addExpense(fromJSON(request.body,account),
                            account,
                            result => JsonResponse(toJSON(result), Nil, Nil, 201)))
  }

 
  // Define an implicit conversion from an Expense to XML or JSON
  import net.liftweb.http.rest.{JsonSelect,XmlSelect}
  implicit def expenseToRestResponse : JxCvtPF[Expense] = {
    case (JsonSelect, e, _) => toJSON(e)
    case (XmlSelect, e, _) => toXML(e)
  }
  
  serveJx {
    case Get(List("api", "expense", Expense(expense,_)), _) => Full(expense)
  }

  // Just an example of autoconversion
  serveJx {
    case Get(List("api", "greet", name),_) => 
      auto(Map("greeting" ->
               Map("who" -> name,
                   "what" -> ("Hello at " + new java.util.Date))))
  }
    
}

// Close package statements
}}
