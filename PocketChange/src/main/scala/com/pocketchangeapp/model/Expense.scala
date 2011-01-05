package com.pocketchangeapp {
package model {

import java.math.MathContext
import java.util.Date
import java.text.DateFormat


import scala.xml.{Elem,NodeSeq,Text}

import net.liftweb.common.{Box,Empty,Full}
import net.liftweb.mapper._

import util.Util

class Expense extends LongKeyedMapper[Expense] with IdPK {
  def getSingleton = Expense

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  // A helper to simplify access to the account name
  def accountName : String = account.obj.map(_.name.is).openOr("unknown")

  object dateOf extends MappedDateTime(this) {
    final val dateFormat = 
      DateFormat.getDateInstance(DateFormat.SHORT)
    override def asHtml = Text(dateFormat.format(is))
  }

  object serialNumber extends MappedLong(this)

  // The amount and balance fields have up to 16 digits and 2 decimal places
  object currentBalance extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  object amount extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  // Holds a brief description of the entry
  object description extends MappedString(this, 100) 

  object notes extends MappedTextarea(this, 1000) {
    override def textareaCols = 60
    override def textareaRows = 8
  }

  object receipt extends MappedBinary(this)
  object receiptMime extends MappedString(this,100)

  private object _dbTags extends HasManyThrough(this, Tag, ExpenseTag, ExpenseTag.expense, ExpenseTag.tag)

  private[model] var _tags : List[Tag] = _

  private val locker = new Object

  def tags : List[Tag] = locker.synchronized {
    if (_tags eq null) {
      _tags = _dbTags()
    }
    _tags
  }

  def tags (newTags : String) = locker.synchronized {
    import net.liftweb.util.StringHelpers.stringToSuper
    _tags = newTags.roboSplit(",").map(Tag.byName(account, _))
    this
  }

  def tags (newTags : List[Tag]) = locker.synchronized {
    _tags = newTags
    this
  }

  def tagsToo : List[Tag] = ExpenseTag.findAll(By(ExpenseTag.expense, this.id)).map(_.tag.obj.open_!)

  def owner = account.obj match {
    case Full(acct) => acct.owner.obj
    case _ => Empty
  }

  override def equals (other : Any) = other match {
    case e : Expense if e.id.is == this.id.is => true
    case _ => false
  }

  override def hashCode = this.id.is.hashCode
}

object Expense extends Expense with LongKeyedMetaMapper[Expense] {
  override def fieldOrder = List(dateOf, amount, description, notes)

  override def afterSave = addTags _ :: Nil

  private def addTags (entry : Expense) {
    if (entry._tags ne null) {
      entry._tags.foreach(ExpenseTag.join(_, entry))
    }
  }

  def getByAcct (account : Account, startDate : Box[Date], endDate : Box[Date], order : Box[OrderBy[Expense,_]], params : QueryParam[Expense]*) : List[Expense] = {
    // Set up some query parameters
    val dateClause : QueryParam[Expense] = (startDate,endDate) match {
      case (Full(start), Full(end)) => BySql("expense.dateOf between ? and ?",
					     IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
					     start, end)
      case (Full(start), Empty) => BySql("expense.dateOf >= ?",
					 IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
					 start)
      case (Empty, Full(end)) => BySql("expense.dateOf <= ?",
				       IHaveValidatedThisSQL("dchenbecker", "2009-10-08"),
				       end)
      case _ => new Ignore[Expense]
    }
    
    val entryOrder : QueryParam[Expense] = order openOr OrderBy(Expense.serialNumber, Descending)


    Expense.findAll((By(Expense.account, account.id) :: dateClause :: entryOrder :: params.toList).toSeq : _*)
  }

  // TODO: getLastExpenseData and updateEntries should be combined to do an atomic update
  // on the Expense table.

  // returns the serial and balance of the last entry before this one
  def getLastExpenseData (acct : Account, date : Date) : (Long,BigDecimal) = {
    // Find the last entry on or before the given date
    val results = getByAcct(acct, Empty, Full(date), Empty, MaxRows(1))

    results match {
      case entry :: Nil => (entry.serialNumber.is, entry.currentBalance.is)
      case Nil => (0,BigDecimal(0))
      case _ => throw new Exception("Invalid prior entry query results") // TODO: handle this better
    }
  }

  val updateString = String.format("update %s set %s = %s + 1, %s = %s + ? where %s >= ?",
				   Expense.dbTableName,
				   Expense.serialNumber.dbColumnName,
				   Expense.serialNumber.dbColumnName,
				   Expense.currentBalance.dbColumnName,
				   Expense.currentBalance.dbColumnName,
				   Expense.serialNumber.dbColumnName)

  /**
   * This method should be called before inserting the new serial number or else you'll get
   * a duplicate serial
   */
  def updateEntries (serial : Long, amount : BigDecimal) = {
    // Simpler to do a bulk update via SQL, unfortunately
    DB.use(DefaultConnectionIdentifier) { conn =>
	DB.prepareStatement(updateString, conn) { stmt =>
	  // pass in the underlying java BigDecimal
	  stmt.setBigDecimal(1, amount.bigDecimal)
	  stmt.setLong(2, serial)
	  stmt.executeUpdate()					 
	}
    }
  }

  import net.liftweb.util.ControlHelpers.tryo
  /**
   * Define an extractor that can be used to locate an Expense based
   * on its ID. Returns a tuple of the Expense and whether the
   * Expense's account is public.
   */
  def unapply (id : String) : Option[(Expense,Boolean)] = tryo {
    find(By(Expense.id, id.toLong)).map { expense => 
      (expense, 
       expense.account.obj.open_!.is_public.is)
    }.toOption
  } openOr None
}
  
class ExpenseTag extends LongKeyedMapper[ExpenseTag] with IdPK {
  def getSingleton = ExpenseTag

  object tag extends MappedLongForeignKey(this, Tag) {
    override def dbIndexed_? = true
  }

  object expense extends MappedLongForeignKey(this, Expense) {
    override def dbIndexed_? = true
  }
}

object ExpenseTag extends ExpenseTag with LongKeyedMetaMapper[ExpenseTag] {
  override def fieldOrder = Nil

  def join (tag : Tag, tx : Expense) = 
    this.create.tag(tag).expense(tx).save

  def findTagExpenses (search : String) : List[Expense] = 
    findAll(In(ExpenseTag.tag,
	       Tag.id,
	       Like(Tag.name, search))).map(_.expense.obj.open_!).distinct
}

// Close package statements 
}}
