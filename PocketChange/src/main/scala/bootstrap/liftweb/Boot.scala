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
    Schemifier.schemify(true, Log.infoF _, User, Tag, Account, AccountAdmin, AccountViewer, AccountNote, Transaction, TransactionTag)

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

    // Set up some rewrites
    LiftRules.rewrite.append {
      case RewriteRequest(ParsePath("viewAcct" :: acctName :: Nil, _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> acctName))
      case RewriteRequest(ParsePath("viewAcct" :: acctName :: tag :: Nil, _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> acctName, "tag" -> tag))
    }

    Configgy.configure("pca.conf")
    val log = Logger.get
    /* Useful? */
    log.info("Configgy up")
    log.info("Bootstrap up")

  }
}

object MenuInfo {
  import Loc._
  val IfLoggedIn = If(() => User.currentUser.isDefined, "You must be logged in")
  def menu: List[Menu] =  Menu(Loc("home", List("index"), "Home")) :: 
    Menu(Loc("manageAccts", List("manage"), "Manage Accounts", IfLoggedIn)) :: 
    Menu(Loc("addAcct", List("editAcct"), "Add Account", Hidden, IfLoggedIn)) ::
    Menu(Loc("viewAcct", List("viewAcct") -> true, "View Account", Hidden, IfLoggedIn)) ::
    User.sitemap
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
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

