package com.pocketchangeapp.model

import net.liftweb._
import mapper._
import util._
import Helpers._

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  private def saved(tag: Tag) {
    synchronized {
      _tagsById = tagsById + (tag.id.is -> tag)
      _tags = tags + (tag.tag.toLowerCase -> tag)
    }
  }
  
  private lazy val allTags = findAll
  
  private var _tags: Map[String, Tag] = _
  
  private var _tagsById: Map[Long, Tag] = _
  
  private def tags = synchronized {
    if (_tags eq null) {
      _tags = Map.empty ++ allTags.map(t => (t.tag.toLowerCase -> t))
    }
    _tags
  }
  
  private def tagsById = synchronized {
    if (_tagsById eq null) {
      _tagsById = Map.empty ++ allTags.map(t => (t.id.is -> t))
    }
    _tagsById
  }
  
  def fromId(id: Int): Can[Tag] = tagsById.get(id)
  
  def idFrom(tag: String): Can[Int] = 
  tags.get(tag.toLowerCase.trim).map(_.id.is.toInt)
  
  def tagFromName(tag: String) = synchronized {
    val ttag = tag.trim
    tags.get(ttag.toLowerCase) match {
      case Some(tag) => tag
      case _ => Tag.create.tag(ttag).saveMe
    }
  }
  
  override def afterSave: List[Tag => Any] = saved _ :: super.afterSave
}

class Tag extends LongKeyedMapper[Tag] {
  def getSingleton = Tag
  def primaryKeyField = id
  
  object id extends MappedLongIndex(this)
  
  object tag extends MappedPoliteString(this, 64) {
    override def setFilter = notNull _ :: trim _ :: super.setFilter 
  }
}

