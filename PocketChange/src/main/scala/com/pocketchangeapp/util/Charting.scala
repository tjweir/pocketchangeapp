package com.pocketchangeapp {
package util {

import scala.collection.mutable.HashMap

import net.liftweb.common.{Box,Empty,Full}
import net.liftweb.http.{ForbiddenResponse,InMemoryResponse,LiftResponse,RedirectResponse}


import java.io.{PipedInputStream,PipedOutputStream}
import java.text.DecimalFormat

import org.jfree.chart.{ChartFactory,ChartUtilities,JFreeChart}
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.general.DefaultPieDataset
import org.jfree.chart.plot.{PiePlot,PlotOrientation}
import org.jfree.chart.title.TextTitle
import org.jfree.chart.labels.{StandardCategoryItemLabelGenerator,StandardPieSectionLabelGenerator}


import model.{Account,Expense,User}

object Charting {
  // We're assuming stateful dispatch here
  private def width = Util.getIntParam("width", 800)
  private def height = Util.getIntParam("height", 200)

  private def withAccount (name : String)(f : Account => Box[LiftResponse]) : Box[LiftResponse] = 
    User.currentUser match {
      case Full(user) => {
	Account.findByName(user, name) match {
	  case acct :: Nil => f(acct)
	  case _ => Empty
	}
      }
      case _ => Full(RedirectResponse("/user_mgt/login")) // Must have a user to access accounts
    }

  private def returnChartPNG (chart : JFreeChart) = {
    val image = chart.createBufferedImage(width, height)
    
    val data = ChartUtilities.encodeAsPNG(image)
					     
    Full(InMemoryResponse(data, List("Content-Type" -> "image/png"), Nil, 200))
  }

  def history (name : String) : Box[LiftResponse] = withAccount(name) {
    acct =>
      val entries = Expense.getByAcct(acct, 
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
	  
    dateMap.keySet.toList.sorted.foreach(key => dataset.addValue(dateMap(key), acct.name.is, key))

    returnChartPNG(ChartFactory.createLineChart("Balance History for " + name,
						"Date",
						"Amount",
						dataset,
						PlotOrientation.VERTICAL, false, false, false))
  }

  private def buildTagChartData (account: Account) = {
    val entries = Expense.getByAcct(account, 
				    Util.getDateParam("start", Util.noSlashDate.parse),
				    Util.getDateParam("end", Util.noSlashDate.parse),
				    Empty)
    
    val tagMap = new HashMap[String,BigDecimal]

    val zero = BigDecimal(0)

    entries.foreach({ entry =>
      entry.tags.map(_.name.is).foreach { 
	tag => tagMap += tag -> (tagMap.getOrElse(tag, zero) + entry.amount.is)
      }
    })

    tagMap
  }

  def tagpie (name : String) : Box[LiftResponse] = withAccount(name) { acct =>
    val dataset = new DefaultPieDataset

    val tags = buildTagChartData(acct)

    tags.keys.toList.sorted.foreach(key => dataset.setValue(key, tags(key)))

    val chart = ChartFactory.createPieChart("Summary by Tag for " + name,
					    dataset,
					    false, false, false)
						  
    chart.addSubtitle(new TextTitle("Entries may have multiple tags, so total may exceed account balance"))

    val itemLabel = new StandardPieSectionLabelGenerator("{0} = {1} ({2})", new DecimalFormat("0.00"), new DecimalFormat("0%"))

    chart.getPlot.asInstanceOf[PiePlot].setLabelGenerator(itemLabel)

    returnChartPNG(chart)
  }
    
  def tagbar (name : String) : Box[LiftResponse] = withAccount(name) { acct =>
    val dataset = new DefaultCategoryDataset

    val tags = buildTagChartData(acct)

    tags.keys.toList.sorted.foreach(key => dataset.addValue(tags(key), name, key))

    val chart = ChartFactory.createBarChart("Summary by Tag for " + name,
					    "Tag",
					    "Total Amount",
					    dataset,
					    PlotOrientation.VERTICAL,
					    false, false, false)
						  
    chart.addSubtitle(new TextTitle("Entries may have multiple tags, so total may exceed account balance"))

    val itemLabel = new StandardCategoryItemLabelGenerator("{0} = {1}", new DecimalFormat("0.00"))

    chart.getCategoryPlot.getRenderer.setBaseItemLabelGenerator(itemLabel)

    returnChartPNG(chart)
  } 
}
    
    
// Close package statements
}}
