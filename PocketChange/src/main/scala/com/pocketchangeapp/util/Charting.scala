package com.pocketchangeapp.util

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.mapper._
import S._
import Helpers._

import scala.collection.mutable.HashMap

import java.io.{PipedInputStream,PipedOutputStream}

import org.jfree.chart.{ChartFactory,ChartUtilities}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.plot.PlotOrientation

import com.pocketchangeapp.model._

object Charting {
  // We're assuming stateful dispatch here
  private def width = Util.getIntParam("width", 800)
  private def height = Util.getIntParam("height", 200)

  def history (name : String) : LiftResponse = User.currentUser match {
    case Full(user) => {
      Account.findByName(user, name) match {
	case acct :: Nil => {
	  val entries = Transaction.getByAcct(acct, 
					      Util.getDateParam("start", Util.noSlashDate.parse),
					      Util.getDateParam("end", Util.noSlashDate.parse),
					      Empty)

	  val serialMap = new HashMap[String,Long]()
	  val dateMap = new HashMap[String,BigDecimal]()

	  // Iterate over entries to find the last entry (and balance) for each date
	  // It would be more efficient to just do this in SQL with a self-join, but then we wouldn't be using mapper...
	  entries.foreach({entry =>
	    val date = Util.slashDate.format(entry.dateOf.is)
	    if (serialMap.getOrElse(date,0l) < entry.serialNumber.is) {
	      serialMap += date -> entry.serialNumber.is
	      dateMap += date -> entry.currentBalance.is
	    }
	  })

	  val dataset = new DefaultCategoryDataset
	  
	  dateMap.keySet.toList.sort(_ < _).foreach(key => dataset.addValue(dateMap(key), acct.name.is, key))

	  val chart = 
	    ChartFactory.createLineChart("Balance History for " + acct.name.is,
					 "Date",
					 "Amount",
					 dataset,
					 PlotOrientation.VERTICAL, false, false, false)

	  val image = chart.createBufferedImage(width, height)

	  val data = ChartUtilities.encodeAsPNG(image)
					     
	  InMemoryResponse(data, List("Content-Type" -> "image/png"), Nil, 200)
	}
	case _ => new NotFoundResponse
      }
    }
    case _ => RedirectResponse("/user_mgt/login") // Must have a user to access accounts
  }

  def tagpie (name : String) = new NotImplementedResponse

  def tagbar (name : String) = new NotImplementedResponse
}
    
    
