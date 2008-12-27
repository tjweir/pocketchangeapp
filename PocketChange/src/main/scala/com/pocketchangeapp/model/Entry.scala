package com.pocketchangeapp.model

import _root_.net.liftweb.mapper._
import DB._
import net.liftweb.util._
import _root_.java.sql.Connection


import net.liftweb._
import mapper._
import util._
import Helpers._
import http._


object Entry extends Entry with KeyedMetaMapper[Long,Entry] {
  override def dbTableName = "entries" // define the DB table name
  override def fieldOrder = List(dateOf, description, amount)

  override def afterSave = addTags _ :: Nil

  private def addTags(e: Entry) {
    if(e._tags ne null) {
      e._tags.foreach {
        t => PCATag.create.entry(e).tag(t).save
      }
    }
  }
}

class Entry extends KeyedMapper[Long, Entry] {
  def getSingleton = Entry
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object dateOf extends MappedDateTime(this)
  object description extends MappedString(this,100)
  
  // 2 digits to the right
  object amount extends MappedDecimal(this,2)

  private[model] var _tags: Array[Int] = _

  def tags(tags: List[Tag]): Entry = {
    _tags = tags.map(_.id.toInt).sort(_ < _).toArray
    this
  }
  
  def tags(tgs: String): Entry = {
    tags(tgs.roboSplit(",").map(Tag.tagFromName))
  }

  def showTags: String = {
    tags.flatMap(i => Tag.fromId(i).map[String](t => t.tag)).reduceLeft(_ + ", " + _)
  } 

  def tags = synchronized {
    if (_tags eq null) {
      _tags = PCATag.findAll(By(PCATag.entry, this)).map(_.tag.toInt).sort(_ < _).toArray
    }
    _tags
  }
}


