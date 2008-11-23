package com.pocketchangeapp.snippet

import scala.xml._
import net.liftweb.http._
import S._
import SHtml._
import scala.xml._

import net.lag.logging.Logger

class Entry {
  val log = Logger.get

  def howdy = <span>Welcome to PocketChange at {new java.util.Date}</span>

  def addForm = {
    log.info("addForm rendered")
    <div>{text("Date", println _)} 
    {text("Description", println _)}
    {text("Tags", println _)}
    {text("Value", println _)}
    {submit("Submit", () => S.notice("Submitted"))}</div>
  }

  def testList: NodeSeq = {
    <table id="" class="" border="0" cellpadding="0" cellspacing="1" width="100%">
      <thead>
	<tr>
	  <th>Date</th>
	  <th>Description</th>
	  <th>Tags</th>
	  <th>Value</th>
	</tr>
      </thead>
      <tbody>
	<tr>
			  
	  <td>Jan 18, 2001 9:12 AM</td>
	  <td>Foo</td>
	  <td>Food, Impulse</td>
	  <td>$9.99</td>
	</tr>
	<tr>
	  <td>Jan 18, 2001 9:12 AM</td>
	  <td>Foo</td>
	  <td>Food, Impulse</td>
	  <td>$9.99</td>
	</tr>
	<tr>
	  <td>Jan 18, 2001 9:12 AM</td>
	  <td>Foo</td>
	  <td>Food, Impulse</td>
	  <td>$9.99</td>
	</tr>
	<tr>
	  <td>Jan 18, 2001 9:12 AM</td>
	  <td>Foo</td>
	  <td>Food, Impulse</td>
	  <td>$9.99</td>
	</tr>

      </tbody>
    </table>
  }

  /*
  def testSwappable: NodeSeq = {
     swappable(<span>Click to edit: <span id='the_text'></span></span>,                                                          
     ajaxText("", 
       v => DisplayMessage("messages", Text("You entered some text: "+v), 4 seconds, 1 second) & SetHtml("the_text", Text(v))))
  }
  */
}

