package com.pocketchangeapp.model

import net.liftweb._
import mapper._
import util._
import Helpers._

object PCATag extends PCATag with LongKeyedMetaMapper[PCATag] {
}

class PCATag extends LongKeyedMapper[PCATag] {
  def getSingleton = PCATag
  def primaryKeyField = id
  
  object id extends MappedLongIndex(this)
  object tag extends MappedLongForeignKey(this, Tag)
  object entry extends MappedLongForeignKey(this, Entry)
}

