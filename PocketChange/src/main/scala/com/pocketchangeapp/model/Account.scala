package com.pocketchangeapp.model

import java.math.MathContext
import java.text.SimpleDateFormat

import net.liftweb.common.{Box,Empty}
import net.liftweb.mapper._

class Account extends LongKeyedMapper[Account] with IdPK {
  def getSingleton = Account

  // Which user created this account?
  object owner extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }

  // Which users, besides the owner, are allowed to modify this account?
  def admins = AccountAdmin.findAll(By(AccountAdmin.account, this.id))

  def addAdmin (user : User) = 
    AccountAdmin.create.account(this).administrator(user).save

  // Which users, besides the owner and admins, are allowed to view this account?
  def viewers = AccountViewer.findAll(By(AccountViewer.account, this.id))

  /* Setting this to true allows anyone to view this account and its activity.
   * This is read-only access. */
  object is_public extends MappedBoolean(this) {
    override def defaultValue = false
  }

  // The balance has up to 16 digits and 2 decimal places
  object balance extends MappedDecimal(this, MathContext.DECIMAL64, 2)

  // The actual expense entries
  def entries = Expense.getByAcct(this, Empty, Empty, Empty)

  // All tags used on expenses for this account
  def tags = Tag.findAll(By(Tag.account, this.id))

  // The name of this account
  object name extends MappedString(this,100)

  // A description of this account
  object description extends MappedString(this, 300)

  // An optional external account identifier
  object externalAccount extends MappedString(this, 300)

  // Optional notes about the account
  def notes = AccountNote.findAll(By(AccountNote.account, this.id))

  // This method checks view access by a particular user
  def isViewableBy (user : Box[User]) : Boolean = {
    is_public.is ||
    user.map(_.allAccounts.contains(this)).openOr(false)
  }

  // This method checks edit access by a particular user
  def isEditableBy (user : Box[User]) : Boolean = 
    user.map(_.editable.contains(this)).openOr(false)
}

object Account extends Account with LongKeyedMetaMapper[Account] {
  def findByName (owner : User, name : String) : List[Account] = 
    Account.findAll(By(Account.owner, owner.id.is), By(Account.name, name))

  import net.liftweb.util.Helpers.tryo
  /**
   * Define an extractor that can be used to locate an Account based
   * on its ID.
   */
  def unapply (id : String) : Option[Account] = tryo {
    find(By(Account.id, id.toLong)).toOption
  } openOr None
}

// Rights classes
class AccountAdmin extends LongKeyedMapper[AccountAdmin] with IdPK {
  def getSingleton = AccountAdmin

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  object administrator extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }
}

object AccountAdmin extends AccountAdmin with LongKeyedMetaMapper[AccountAdmin] {
}

class AccountViewer extends LongKeyedMapper[AccountViewer] with IdPK {
  def getSingleton = AccountViewer

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  object viewer extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }
}

object AccountViewer extends AccountViewer with LongKeyedMetaMapper[AccountViewer]

// Extra
class AccountNote extends LongKeyedMapper[AccountNote] with IdPK {
  def getSingleton = AccountNote

  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }

  object note extends MappedText(this)
}

object AccountNote extends AccountNote with LongKeyedMetaMapper[AccountNote] {
  override def fieldOrder = note :: Nil
}
