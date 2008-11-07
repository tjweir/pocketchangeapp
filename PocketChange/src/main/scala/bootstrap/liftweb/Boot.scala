package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import com.pocketchangeapp.model._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    // where to search snippet
    LiftRules.addToPackages("com.pocketchangeapp")     
    Schemifier.schemify(true, Log.infoF _, User)


    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))
  }
}

object MenuInfo {
  import Loc._

  def menu: List[Menu] =  Menu(Loc("home", List("index"), "Home")) ::
  Menu(Loc("xml fun", List("xml_fun"), "XML Fun")) ::
  Menu(Loc("chat", List("chat"), "Comet Chat")) ::
  Menu(Loc("database", List("database"), "Database")) ::
  Menu(Loc("ajax", List("ajax"), "AJAX Samples")) ::
  Menu(Loc("ajax form", List("ajax-form"), "AJAX Form")) ::
  Menu(Loc("json", List("json"), "JSON Messaging")) ::
  Menu(Loc("template", List("template"), "Templates")) ::
  Menu(Loc("ws", List("ws"), "Web Services")) ::
  Menu(Loc("file_upload", List("file_upload"), "File Upload")) ::
  // Menu(Loc("wiki", Link(List("wiki"), true, "/wiki/HomePage"), "Wiki")) ::
  Nil
}



object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm = DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}

