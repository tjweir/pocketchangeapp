/*
 * RestAPI.scala
 *
 * Copyright 2008-2010 Derek Chen-Becker, Marius Danciu and Tyler Wier
 * 
 */
package com.pocketchangeapp {
package api {

import java.text.SimpleDateFormat

import scala.xml.{Elem, NodeSeq, Text}

import net.liftweb.common.{Box,Empty,Failure,Full}
import net.liftweb.mapper.{By,MaxRows}
import net.liftweb.json.JsonAST.{JObject,JValue}
import net.liftweb.http.js.JsExp

import model._

/**
 * This object provides some conversion and formatting specific to our
 * REST API.
 */
object RestFormatters {
  /* The REST timestamp format. Not threadsafe, so we create
   * a new one each time. */
  def timestamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  // A simple helper to generate the REST ID of an Expense
  def restId (e : Expense) = 
    "http://www.pocketchangeapp.com/api/expense/" + e.id

  // A simple helper to generate the REST timestamp of an Expense
  def restTimestamp (e : Expense) : String = 
    timestamp.format(e.dateOf.is)

  /**
   * Generates the XML REST representation of an Expense
   */
  def toXML (e : Expense) : Elem = 
    <expense>
      <id>{restId(e)}</id>
      <accountname>{e.accountName}</accountname>
      <accountid>{e.account.obj.open_!.id.is}</accountid>
      <date>{restTimestamp(e)}</date>
      <description>{e.description.is}</description>
      <amount>{e.amount.is.toString}</amount>
      <tags>
        {e.tags.flatMap {t => <tag>{t.name.is}</tag>}}
      </tags>
    </expense>

  /**
   * Generates the JSON REST representation of an Expense
   * as a JSON JValue
   */
  def toJSON (e : Expense) : JValue = {
    import net.liftweb.json.JsonDSL._
    import net.liftweb.json.JsonAST._

    ("id" -> restId(e)) ~
    ("date" -> restTimestamp(e)) ~
    ("description" -> e.description.is) ~
    ("accountname" -> e.accountName) ~
    ("accountid" -> e.account.obj.open_!.id.is) ~
    ("amount" -> e.amount.is.toString) ~
    ("tags" -> e.tags.map(_.name.is))
  }

  /**
   * Wraps the JSON JValue representation of an Expense
   * in a JsRaw element.
   */
  def toJSONExp (e : Expense) : JsExp = {
    import net.liftweb.http.js.JE.JsRaw
    import net.liftweb.json.JsonDSL._
    import net.liftweb.json.JsonAST._
    
    JsRaw(compact(render(toJSON(e))))
  }

  /*
   * Generates an Atom 1.0 feed from the last 10 Expenses for the given
   * account.
   */
  def toAtom (a : Account) : Elem = {
    val entries = Expense.getByAcct(a,Empty,Empty,Empty,MaxRows(10))
    
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>{a.name}</title>
      <id>urn:uuid:{a.id.is}</id>
      <updated>{entries.headOption.map(restTimestamp) getOrElse
                timestamp.format(new java.util.Date)}</updated>
      { entries.flatMap(toAtom) }
    </feed>
  }

  /*
   * Generates the XML Atom representation of an Expense
   */
  def toAtom (e : Expense) : Elem = 
    <entry>
      <id>urn:uuid:{restId(e)}</id>
      <title>{e.description.is}</title>
      <updated>{restTimestamp(e)}</updated>
      <content type="xhtml">
        <div xmlns="http://www.w3.org/1999/xhtml">
          <table>
          <tr><th>Amount</th><th>Tags</th><th>Receipt</th></tr>
          <tr><td>{e.amount.is.toString}</td>
              <td>{e.tags.map(_.name.is).mkString(", ")}</td>
              <td>{ 
                if (e.receipt.is ne null) { 
                  <img src={"/image/" + e.id} /> 
                } else Text("None") 
              }</td></tr>
          </table>
        </div>
      </content>
    </entry>

  /**
   * Parses an Expense from a Map of string/value pairs.This method does
   * only rudimentary validation of the parsed Expense and tries to be
   * lenient on input.
   */
  def fromMap (data : scala.collection.Map[String,String], tags: List[String], account : Account) : Box[Expense] = {
    val expense = Expense.create

    try {
      val fieldParsers : List[(String, String => Expense)] = 
        ("date", (date : String) => expense.dateOf(timestamp.parse(date))) ::
        ("description", (desc : String) => expense.description(desc)) ::
        ("amount", (amount : String) => expense.amount(BigDecimal(amount))) :: Nil

      val missing = fieldParsers.flatMap { 
        field => // We invert the flatMap here to only give us missing values
          if (data.get(field._1).map(field._2).isDefined) None else Some(field._1)
      }
      
      if (missing.isEmpty) {
        expense.account(account)
        expense.tags(tags.map(Tag.byName(account.id.is,_)))
        Full(expense)
      } else {
        Failure(missing.mkString("Invalid expense. Missing: ", ",", ""))
      }
    } catch {
      case pe : java.text.ParseException => Failure("Failed to parse date")
      case nfe : java.lang.NumberFormatException => Failure("Failed to parse amount")
    }
  }

  /**
   * Parses an Expense from JSON input.
   */
  def fromJSON (obj : Box[Array[Byte]], account : Account) : Box[Expense] = obj match {
    case Full(rawBytes) => {
      // We use the Scala util JSON parser here because we want to avoid parsing
      // numeric values into doubles. We'll just leave them as Strings
      import scala.util.parsing.json.JSON
      JSON.perThreadNumberParser = { in : String => in }
      
      val contents = new String(rawBytes, "UTF-8")
      JSON.parseFull(contents) match {
        case Some(data : Map[String,Any]) => {
          val tags : List[String] = data.get("tags") match {
            case Some(l : List[String]) => l
            case _ => Nil
          }
          
          val otherData = 
            data.filterKeys(_ != "tags").mapElements(_.toString)

          fromMap(otherData, tags, account)
        }
        case other => Failure("Invalid JSON submitted: \"%s\"".format(contents))
      }
    }
    case _ => Failure("Empty body submitted")
  }

  /**
   * Parses an Expense from XML input.
   */
  def fromXML (rootNode : Box[Elem], account : Account) : Box[Expense] = rootNode match {
    case Full(<expense>{parameters @ _*}</expense>) => {
      var data = Map[String,String]()
      var tags = List[String]()

      for(parameter <- parameters) { 
        parameter match {
          case <date>{date}</date> => data += "date" -> date.text
          case <description>{description}</description> => 
            data += "description" -> description.text
          case <amount>{amount}</amount> => data += "amount" -> amount.text
          case <tags>{ tagElems @ _* }</tags> => {
            for (tag <- tagElems) {
              tag match {
                case <tag>{tagName}</tag> => tags ::= tagName.text
                case other => // Ignore (could be whitespace)
              }
            }
          }
          case _ => // Ignore (could be whitespace)
        }
      }
      
      fromMap(data, tags, account)
    }
    case other => Failure("Missing root expense element")
  }
}

// Close package statements
}}
