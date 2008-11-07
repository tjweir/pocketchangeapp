package com.pocketchangeapp.model

import _root_.net.liftweb.mapper._
import DB._
import net.liftweb.util._
import _root_.java.sql.Connection

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with KeyedMetaMapper[Long, User] {
  override def dbTableName = "users" // define the DB table name

  // define the order fields will appear in forms and output
  override def fieldOrder = id :: firstName :: lastName :: email ::
  password :: Nil
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends ProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  
}
