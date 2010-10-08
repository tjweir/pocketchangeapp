/*
 * RestAPI.scala
 */
package com.pocketchangeapp {
package api {

import java.text.SimpleDateFormat

import scala.xml.{Elem, Node, NodeSeq, Text}

import net.liftweb.common.{Box,Empty,Full,Logger}
import net.liftweb.http.{AtomResponse,BadResponse,CreatedResponse,GetRequest,JsonResponse,LiftResponse,LiftRules,NotFoundResponse,ParsePath,PutRequest,Req,RewriteRequest}
import net.liftweb.http.rest.XMLApiHelper
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.mapper.{By,MaxRows}

import model._

/**
 * This object provides some conversion and formatting specific to our
 * REST API.
 */
object RestFormatters {
  // A simple helper to generate the REST ID of an Expense
  def restId (e : Expense) = "http://www.pocketchangeapp.com/api/expense/" + e.id

  // A simple helper to generate the REST timestamp of an Expense
  def restTimestamp (e : Expense) : String = 
    (new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")).format(e.dateOf.is)

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

  /*
   * Generates the JSON REST representation of an Expense
   */
  def toJSON (e : Expense) : JsExp = {
    import net.liftweb.json.JsonDSL._
    import net.liftweb.json.JsonAST._

    val entry  = 
      ("id" -> restId(e)) ~
      ("date" -> restTimestamp(e)) ~
      ("description" -> e.description.is) ~
      ("accountname" -> e.accountName) ~
      ("amount" -> e.amount.is.toString) ~
      ("tags" -> e.tags.map(_.name.is))
    
    JsRaw(compact(render(entry)))
  }

  /*
   * Generates an Atom 1.0 feed from the last 10 Expenses for the given
   * account.
   */
  def toAtom (a : Account) : Elem = {
    val entries = Expense.getByAcct(a,Empty,Empty,Empty,MaxRows(10))
    
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>{a.name}</title>
      <id>urn:uuid:{a.uuid.is}</id>
      <updated>{a.entries.headOption.map(restTimestamp) getOrElse
                (new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")).format(new java.util.Date)}</updated>
      { a.entries.flatMap(toAtom) }
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
              <td>{ if (e.receipt.is ne null) { <img src={"/image/" + e.id} /> } else Text("None") }</td></tr>
          </table>
        </div>
      </content>
    </entry>
}

object DispatchRestAPI extends XMLApiHelper {
  // Import our implicits for converting things around
  import RestFormatters._

  def dispatch: LiftRules.DispatchPF = {     
    // Define our getters first
    case Req(List("api", "expense", Expense(e)), _, GetRequest) => 
      () => Full(nodeSeqToResponse(toXML(e))) // default to XML
    case Req(List("api", "expense", Expense(e), "xml"), _, GetRequest) => 
      () => Full(nodeSeqToResponse(toXML(e))) // explicitly XML
    case Req(List("api", "expense", Expense(e), "json"), _, GetRequest) => 
      () => JsonResponse(toJSON(e), Nil, Nil, 200)  // explicitly JSON
    case Req(List("api", "account", Account(a)), _, GetRequest) =>
      () => AtomResponse(toAtom(a)) // explicit atom request

    // Define the PUT handler
    case r @ Req("api" :: "expense" :: Nil, _, PutRequest) => () => addExpense(r)

    // Invalid API request - route to our error handler
    case Req("api" :: x :: Nil, "", _) => () => BadResponse() // Everything else fails
  }

  def createTag (xml : NodeSeq) : Elem = <pca_api>{xml}</pca_api>

  // reacts to the PUT Request
  def addExpense(req: Req): LiftResponse = {
    var tempEmail = ""
    var tempPass = ""
    var tempAccountUUID = ""

    var expense = new Expense
    req.xml match {
      case Full(<expense>{parameters @ _*}</expense>) => {
        for(parameter <- parameters){ parameter match {
          case <email>{email}</email> => tempEmail = email.text
          case <password>{password}</password> => tempPass = password.text
          case <accountUUID>{uuid}</accountUUID> => tempAccountUUID = uuid.text
          case <dateOf>{dateof}</dateOf> => expense.dateOf(new java.util.Date(dateof.text))
          case <amount>{value}</amount> => expense.amount(BigDecimal(value.text))
          case <desc>{description}</desc> => expense.description(description.text)
          case _ =>
        }
      }

      try {
        val u:User = User.find(By(User.email, tempEmail)) match {
          case Full(user) if user.validated && user.password.match_?(tempPass) =>
            User.logUserIn(user)
            user
          case _ => new User
        }

        val currentAccount = Account.find(By(Account.owner, u.id.is), By(Account.uuid, tempAccountUUID)).open_!
        expense.account(currentAccount.id.is)

        val (entrySerial,entryBalance) = Expense.getLastExpenseData(currentAccount, expense.dateOf)

        expense.account(currentAccount).serialNumber(entrySerial + 1).tags("api").currentBalance(entryBalance + expense.amount)

        expense.validate match {
          case Nil =>
            Expense.updateEntries(entrySerial + 1, expense.amount.is)
            expense.save
            println(currentAccount.name)
            //val acct = Account.find(currentAccount.name.is).open_!
            val newBalance = currentAccount.balance.is + expense.amount.is
            currentAccount.balance(newBalance).save

            CreatedResponse(expense.toXml, "text/xml")
          case _ =>
            BadResponse() // TODO: Return a meaningful error
        }
      }
      catch {
        case e => Logger(this.getClass).error("Could not add expense", e); BadResponse()
      }
    }
    case _ => Logger(this.getClass).error("Request was malformed"); BadResponse()
    }
  }

}


// Close package statements
}}
