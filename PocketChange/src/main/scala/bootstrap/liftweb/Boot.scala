package bootstrap.liftweb

import net.liftweb.common.{Box,Empty,Failure,Full,Logger}
import net.liftweb.util.Helpers
import net.liftweb.http.{LiftRules,ParsePath,Req,RewriteRequest,RewriteResponse}
import net.liftweb.sitemap.{Loc,Menu,SiteMap}
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import com.pocketchangeapp.model._
import com.pocketchangeapp.api._
import com.pocketchangeapp.util.{Charting,Image}

// Get implicit conversions 
import net.liftweb.sitemap.Loc._
import net.liftweb.util.Helpers._

 
/**
 * The bootstrap.liftweb.Boot class is the main entry point for Lift.
 * This is where all of the initial App setup is performed. In particular,
 * the Boot.boot method is what lift calls after it loads.
 * 
 * TODO: Connect Lucene/Compass for search
 */
class Boot {
  def boot {
    /*
     * LiftRules.early allows us to apply functions to the request before
     * Lift has started to work with it. In our case, we're explicitly
     * setting the character encoding to work around an issue with
     * Jetty not properly discovering the encoding from the client.
     */
    LiftRules.early.append {
      _.setCharacterEncoding("UTF-8")
    }

    /*
     * If you're using reflection-based dispatch in Lift, then you need to
     * tell Lift which packages contain the View/Snippet/Comet classes.
     */
    LiftRules.addToPackages("com.pocketchangeapp")


    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)


    def schemeLogger (msg : => AnyRef) = {
      Logger(classOf[Boot]).info(msg)
    }

    Schemifier.schemify(true, schemeLogger _, User, Tag, Account, AccountAdmin, AccountViewer, AccountNote, Expense, ExpenseTag)

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

    LiftRules.dispatch.prepend(RestAPI.dispatch)

    // Set up some rewrites
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("account", acctName), _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> urlDecode(acctName)))
      case RewriteRequest(ParsePath(List("account", acctName, tag), _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> urlDecode(acctName), "tag" -> urlDecode(tag)))
    }

    // Custom dispatch for graph and receipt image generation
    LiftRules.dispatch.append {
      case Req(List("graph", acctName, "history"), _, _) =>
	() => Charting.history(acctName)
      case Req(List("graph", acctName, "tagpie"), _, _) =>
	() => Charting.tagpie(acctName)
      case Req(List("graph", acctName, "tagbar"), _, _) =>
	() => Charting.tagbar(acctName)
      case Req(List("image", expenseId), _, _) =>
	() => Full(Image.viewImage(expenseId))
    }

    import scala.xml.Text
    val m = Title(if (User.loggedIn_?) { Text("a") } else { Text("b") })

    Logger(classOf[Boot]).info("Bootstrap up")
  }
}

object MenuInfo {
  import Loc._
  val IfLoggedIn = If(() => User.currentUser.isDefined, "You must be logged in")

  def menu: List[Menu] = 
    List[Menu](Menu("Home") / "index",
               Menu("Manage Accounts") / "manage" >> IfLoggedIn,
               Menu("Add Account") / "editAcct" >> Hidden >> IfLoggedIn,
               Menu(Loc("viewAcct", List("viewAcct") -> true, "View Account", Hidden, IfLoggedIn)),
               Menu("Help") / "help" / "index") :::
    User.sitemap
  
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
    try {
      /** Uncomment if you really want Derby
       * 
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm = DriverManager.getConnection("jdbc:derby:pca_example;create=true")
      */

      Class.forName("org.h2.Driver")
      val dm = DriverManager.getConnection("jdbc:h2:pca_example")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}

