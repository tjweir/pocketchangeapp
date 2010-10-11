package com.pocketchangeapp.model

import net.liftweb._
import mapper._
import util._
import Helpers._

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  def byName (accountId : Long, name : String) = 
    findAll(By(Tag.account, accountId), By(Tag.name, name.trim)) match {
      case tag :: rest => tag
      // create a tag for the given name if it doesn't exist... 
      case Nil => Tag.create.name(name).account(accountId).saveMe
    }
}

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton = Tag
  
  object name extends MappedPoliteString(this, 64) {
    override def setFilter = notNull _ :: trim _ :: super.setFilter 
  }

  // Each tag belongs to a specific account.
  object account extends MappedLongForeignKey(this, Account) {
    override def dbIndexed_? = true
  }
}

