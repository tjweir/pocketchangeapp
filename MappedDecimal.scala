package net.liftweb.mapper

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import _root_.java.sql.{ResultSet, Types}
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.util.Date
import _root_.net.liftweb.http._
import _root_.scala.xml.NodeSeq
import js._

import java.math.BigDecimal
import java.math.RoundingMode

class MappedDecimal[T <: Mapper[T]] (val fieldOwner : T, scale : Int) extends MappedField[BigDecimal,T] {
  def this(rec : T, newVal : BigDecimal) = {
    this(rec, newVal.scale)
    set(newVal)
  }
  
  var rounding = RoundingMode.HALF_EVEN

  //def defaultValue = (new BigDecimal("0")).setScale(scale)
  var zero = new BigDecimal("0")  
  def defaultValue = zero.setScale(scale)
  
  def dbFieldClass = classOf[BigDecimal]

  private var data : BigDecimal = defaultValue
  private var orgData : BigDecimal = defaultValue
  private def st (in : BigDecimal) {
    data = in
    orgData = in
  }
  protected def i_is_! = data
  protected def i_was_! = orgData
  override def doneWithSave() {
    orgData = data
  }


 override def readPermission_? = true
  override def writePermission_? = true
  protected def i_obscure_!(in : BigDecimal) = defaultValue
  protected def real_i_set_!(value : BigDecimal): BigDecimal = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }


	def asJsExp = JE.Num(is)

def setFromAny (in : Any) : BigDecimal =
    in match {
      case n :: _ => setFromString(n.toString)
      case Some(n) => setFromString(n.toString)
      case Full(n) => setFromString(n.toString)
      case None | Empty | Failure(_, _, _) | null => setFromString("0")
      case n => setFromString(n.toString)
    }

  def setFromString (in : String) : BigDecimal = {
    this.setAll(new BigDecimal(in))
  }

  protected def setAll (in : BigDecimal) = set(in.setScale(scale, rounding))

	//def real_convertToJDBCFriendly(value: BigDecimal): Object = new _root_.java.math.BigDecimal(value.toString)
	def real_convertToJDBCFriendly(value: BigDecimal): Object = value
  
  def targetSQLType = Types.DECIMAL
  
  def jdbcFriendly(field : String) = i_is_!

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit = (inst, v) => 
    doField(inst, accessor, {case f: MappedDecimal[T] => f.st(if (v == null) defaultValue else (new BigDecimal(v.getTime).setScale(scale)))})        
    
  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(new BigDecimal(v).setScale(scale))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>
    Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(if (isNull) defaultValue else (new BigDecimal(v).setScale(scale)))})      
      
  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>
    Unit = (inst, v) => doField(inst, accessor, {case f: MappedDecimal[T] => f.st(new BigDecimal(v.toString).setScale(scale))})

  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " DECIMAL(20," + scale + ")" 

  def += (other : BigDecimal) = setAll(data.add(other))
  def -= (other : BigDecimal) = setAll(data.subtract(other))
  def *= (other : BigDecimal) = setAll(data.multiply(other))
  def /= (other : BigDecimal) = setAll(data.divide(other))
}
                                                             
