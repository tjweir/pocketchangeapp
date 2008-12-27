package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import com.pocketchangeapp.model._
import net.lag.configgy.Configgy
import net.lag.logging.Logger
 
/* Connect Lucene/Compass for search */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    LiftRules.addToPackages("com.pocketchangeapp")     
    Schemifier.schemify(true, Log.infoF _, User, Entry, Tag, PCATag)

    Configgy.configure("pca.conf")
    val log = Logger.get
    /* Useful? */
    log.info("Configgy up")
    log.info("Bootstrap up")

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))
  }
}

object MenuInfo {
  import Loc._
  def menu: List[Menu] =  Menu(Loc("home", List("index"), "Home")) :: Nil
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.postgresql.Driver")
      val dm = DriverManager.getConnection("jdbc:postgresql://localhost/pca", "pca", "pca")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}

