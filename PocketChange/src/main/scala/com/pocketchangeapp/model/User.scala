package com.pocketchangeapp.model

import _root_.net.liftweb.mapper._
import DB._
import net.liftweb.util._
import _root_.java.sql.Connection

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  
  override def skipEmailValidation = true

  // Spruce up the forms a bit
  override def loginXhtml =
    <lift:surround with="default" at="content">
      { super.loginXhtml }
    </lift:surround>

  override def signupXhtml(user: User) = 
    <lift:surround with="default" at="content">
      { super.signupXhtml(user) }
    </lift:surround>

  // define the order fields will appear in forms and output
  //override def fieldOrder = id :: firstName :: lastName :: email :: password :: Nil
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  
  def accounts : List[Account] = Account.findAll(By(Account.owner, this.id))

  def administered : List[Account] = AccountAdmin.findAll(By(AccountAdmin.admin, this.id)).map(_.account.obj.open_!)

  def editable = accounts ++ administered
    
  def viewed : List[Account] = AccountViewer.findAll(By(AccountViewer.viewer, this.id)).map(_.account.obj.open_!)

  def allAccounts : List[Account] = accounts ::: administered ::: viewed
}
