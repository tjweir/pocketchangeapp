package com.pocketchangeapp.snippet

import scala.xml._
import net.liftweb.http._
import S._
import SHtml._
import scala.xml._

import com.pocketchangeapp.model._

import java.math.BigDecimal
import java.util.Date

import net.lag.logging.Logger

class HomePage {
  val log = Logger.get

  def howdy = <span>Welcome to PocketChange at {new java.util.Date}</span>

  val formatter = new java.text.SimpleDateFormat("yyyy/MM/dd")

  def addForm = {
    log.info("addForm rendered")
    <div>{text("Date", println _)} 
    {text("Description", println _)}
    {text("Tags", println _)}
    {text("Value", println _)}
    {submit("Submit", () => S.notice("Submitted"))}</div>
  }

  def addAnEntry: NodeSeq = {
    val e = (new Entry).dateOf(new java.util.Date).description("Food").amount(new BigDecimal("12.43"))
    e.save

    <h1>Adding an entry</h1>
  }

  def testList: NodeSeq = {
    val entries = Entry.findAll
    <table id="" class="" border="0" cellpadding="0" cellspacing="1" width="100%">
      <thead>
	      <tr>
	        <th>Date</th><th>Description</th><th>Tags</th><th>Value</th>
	      </tr>
      </thead>
      <tbody>
      { entries.map(x => <tr>
        <td>{formatter.format(x.dateOf.is)}</td>
        <td>{x.description.is.toString}</td>
        <td>{x.showTags}</td>
        <td>${x.amount.is.toString}</td>
        </tr>
        )}
      </tbody>
    </table>
  }
}

