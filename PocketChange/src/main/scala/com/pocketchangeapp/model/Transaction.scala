/**
 Insert copyright boilerplate here
*/

package com.pocketchangeapp.model

import _root_.java.math.MathContext

import _root_.net.liftweb.mapper._
import _root_.scala.xml.Text

import _root_.net.liftweb.util.Helpers._

class Transaction extends LongKeyedMapper[Transaction] with IdPK {
  def getSingleton = Transaction

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  object dateOf extends MappedDateTime(this)

  // The amount has up to 16 digits and 2 decimal places
  object amount extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  // Holds a brief description of the transaction
  object summary extends MappedString(this, 100) 

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
  override def fieldOrder = List(dateOf, amount, summary, notes)

  override def afterSave = addTags _ :: Nil

  private def addTags (tx : Transaction) {
    if (tx._tags ne null) {
      tx._tags.foreach(TransactionTag.create.transaction(tx).tag(_).save)
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
