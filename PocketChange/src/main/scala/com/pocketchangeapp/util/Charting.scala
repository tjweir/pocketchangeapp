package com.pocketchangeapp.util

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import S._
import Helpers._

import java.io.{PipedInputStream,PipedOutputStream}

import org.jfree.chart.{ChartFactory,ChartUtilities}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation

import com.pocketchangeapp.model._

object Charting {
  private def dateClauses = {
    (S.param("start") match {
      case Full(start) => By_>(Transaction.dateOf, Util.noSlashDate.parse(start)) :: Nil
      case _ => Nil
    }) :::
    (S.param("end") match {
      case Full(end) => By_<(Transaction.dateOf, Util.noSlashDate.parse(end)) :: Nil
      case _ => Nil
    })
  }

  // We're assuming stateful dispatch here

  private def width = Util.getIntParam("width", 640)
  private def height = Util.getIntParam("height", 480)

  def history (name : String) : LiftResponse = User.currentUser match {
    case Full(user) => {
      Account.findByName(user, name) match {
	case acct :: Nil => {
	  val entries = Transaction.findAll((By(Transaction.account, acct.id) :: OrderBy(Transaction.serialNumber, Ascending) :: dateClauses) : _*)

	  val dataset = new DefaultCategoryDataset
	  
	  entries.foreach(entry => dataset.addValue(entry.currentBalance.is, entry.dateOf.toString + entry.id.toString, acct.name.is))

	  // Set up pipes to stream the chart
	  // TODO: how to best handle I/O exception on pipe creation?
	  val chartOutput = new PipedOutputStream()
	  val chartInput = new PipedInputStream(chartOutput)

	  // Fire up chart writer in new thread so that we don't deadlock
	  (new Thread {
	    override def run {
	      val chart = 
		ChartFactory.createLineChart("Balance History for " + acct.name.is,
					     "Date",
					     "Amount",
					     dataset,
					     PlotOrientation.HORIZONTAL, false, false, false)

	      ChartUtilities.writeChartAsPNG(chartOutput, chart, width, height)
	    }
	  }).start
					     
	  StreamingResponse(chartInput,() => { chartInput.close(); chartOutput.close() }, 0, List("Content-Type" -> "image/png"), Nil, 200)
	}
	case _ => new NotFoundResponse
      }
    }
    case _ => RedirectResponse("/user_mgt/login") // Must have a user to access accounts
  }

  def tagpie (name : String) = new NotImplementedResponse

  def tagbar (name : String) = new NotImplementedResponse
}
    
    
