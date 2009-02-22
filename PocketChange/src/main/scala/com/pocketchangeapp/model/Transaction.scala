/**
 Insert copyright boilerplate here
*/

package com.pocketchangeapp.model

import _root_.java.math.MathContext
import _root_.java.util.Date

import _root_.net.liftweb.mapper._
import _root_.scala.xml.Text

import _root_.net.liftweb.util.Helpers._

import com.pocketchangeapp.util.Util

class Transaction extends LongKeyedMapper[Transaction] with IdPK {
  def getSingleton = Transaction

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  object dateOf extends MappedDateTime(this)

  object serialNumber extends MappedLong(this)

  // The amount and balance fields have up to 16 digits and 2 decimal places
  object currentBalance extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  object amount extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  // Holds a brief description of the transaction
  object description extends MappedString(this, 100) 

  object notes extends MappedTextarea(this, 1000) {
    override def textareaCols = 60
    override def textareaRows = 8
  }

  private object _dbTags extends HasManyThrough(this, Tag, TransactionTag, TransactionTag.transaction, TransactionTag.tag)

  private[model] var _tags : List[Tag] = _

  private val locker = new Object

  def tags : List[Tag] = locker.synchronized {
    if (_tags eq null) {
      _tags = _dbTags()
    }
    _tags
  }

  def tags (newTags : String) = locker.synchronized {
    _tags = newTags.roboSplit(",").map(Tag.byName(account, _))
    this
  }

  def tags (newTags : List[Tag]) = locker.synchronized {
    _tags = newTags
    this
  }

  def showTags = Text(tags.map(_.tag.is).mkString(", "))
}

object Transaction extends Transaction with LongKeyedMetaMapper[Transaction] {
  override def dbTableName = "transactions"
  override def fieldOrder = List(dateOf, amount, description, notes)

  override def afterSave = addTags _ :: Nil

  private def addTags (tx : Transaction) {
    if (tx._tags ne null) {
      tx._tags.foreach(TransactionTag.create.transaction(tx).tag(_).save)
    }
  }
  
  // returns the serial and balance of the last entry before this one
  def getLastEntryData (date : Date) : (Long,BigDecimal) = {
    // Find the last transaction on or before the given date
    val results = Transaction.findAll(
      BySql(String.format("%s.%s <= '%s'",
			  Transaction.dbTableName,
			  Transaction.dateOf.dbColumnName,
			  Util.noSlashDate.format(date)),
	    IHaveValidatedThisSQL("dchenbecker", "2009-02-21")),
      OrderBy(Transaction.dateOf, Descending),
      MaxRows(1))

    results match {
      case entry :: Nil => (entry.serialNumber.is, entry.currentBalance.is)
      case Nil => (0,BigDecimal(0))
      case _ => throw new Exception("Invalid prior entry query results") // TODO: handle this better
    }
  }

  val updateString = String.format("update %s set %s = %s + 1, %s = %s + ? where %s >= ?",
				   Transaction.dbTableName,
				   Transaction.serialNumber.dbColumnName,
				   Transaction.serialNumber.dbColumnName,
				   Transaction.currentBalance.dbColumnName,
				   Transaction.currentBalance.dbColumnName,
				   Transaction.serialNumber.dbColumnName)

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
}
  
class TransactionTag extends LongKeyedMapper[TransactionTag] with IdPK {
  def getSingleton = TransactionTag

  object tag extends MappedLongForeignKey(this, Tag) {
    override def dbIndexed_? = true
  }

  object transaction extends MappedLongForeignKey(this, Transaction) {
    override def dbIndexed_? = true
  }
}

object TransactionTag extends TransactionTag with LongKeyedMetaMapper[TransactionTag] {
  override def dbTableName = "transaction_tags"
  override def fieldOrder = Nil
}
