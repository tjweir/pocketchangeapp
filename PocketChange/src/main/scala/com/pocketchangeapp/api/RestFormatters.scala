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
import net.liftweb.json.JsonAST.JValue
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
      <date>{restTimestamp(e)}</date>
      <description>{e.description.is}</description>
      <amount>{e.amount.is.toString}</amount>
      <tags>
        {e.tags.flatMap {t => <tag>{t.name.is}</tag>}}
      </tags>
    </expense>

  /**
   * Parses an Expense from XML input. This method does only rudimentary
   * validation of the parsed Expense and tries to be lenient on input.
   */
  def fromXML (rootNode : Box[Elem], account : Account) : Box[Expense] = rootNode match {
    case Full(<expense>{parameters @ _*}</expense>) => {
      val expense = Expense.create

      try {
        // Keep track of what we've parse so that we can validate
        var (dateSet,descriptionSet,amountSet) = (false,false,false)

        for(parameter <- parameters) { 
          parameter match {
            case <date>{date}</date> => 
              expense.dateOf(timestamp.parse(date.text)); dateSet = true
            case <description>{description}</description> => 
              expense.description(description.text); descriptionSet = true
            case <amount>{value}</amount> => 
              expense.amount(BigDecimal(value.text)); amountSet = true
            case <tags>{ tagElems @ _* }</tags> => {
              var tags = List[String]()
              
              for (tag <- tagElems) {
                tag match {
                  case <tag>{tagName}</tag> => tags ::= tagName.text
                  case other => // Ignore (could be whitespace)
                }
              }

              expense.tags(tags.map(Tag.byName(account.id.is, _)))
            }
            case _ => // Ignore (could be whitespace)
          }
        }
      
        if (dateSet && descriptionSet && amountSet) {
          Full(expense)
        } else {
          val missing = 
            List(dateSet,descriptionSet,amountSet).
              zip(List("date","description","amount")).
              filter(!_._1).map(_._2).mkString(",")

          Failure("Invalid Expense XML. Missing: " + missing)
        }
      } catch {
        case pe : java.text.ParseException =>
          Failure("Failed to parse date")
        case nfe : java.lang.NumberFormatException =>
          Failure("Failed to parse amount")
      }
    }
    case other => Empty
  }


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
}

// Close package statements
}}
