package com.pocketchangeapp.model

import net.liftweb.common.Full

import net.liftweb.mapper.{By,MegaProtoUser,MetaMegaProtoUser}

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  
  // Just for testing purposes. In production we remove this
  override def skipEmailValidation = true

  // Spruce up the forms a bit
  override def screenWrap = 
    Full(<lift:surround with="default" at="content"><div id="formBox"><lift:bind /></div></lift:surround>)

  // define the order fields will appear in forms and output
  //override def fieldOrder = id :: firstName :: lastName :: email :: password :: Nil
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  
  def accounts : List[Account] = Account.findAll(By(Account.owner, this.id))

  def administered : List[Account] = AccountAdmin.findAll(By(AccountAdmin.administrator, this.id)).map(_.account.obj.open_!)

  def editable = accounts ++ administered
    
  def viewed : List[Account] = AccountViewer.findAll(By(AccountViewer.viewer, this.id)).map(_.account.obj.open_!)

  def allAccounts : List[Account] = accounts ::: administered ::: viewed
}
