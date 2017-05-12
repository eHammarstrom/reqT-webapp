package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.streams.ActorFlow
import play.api.mvc._
//import shared.{ Class => Clazz, _}
import upickle.default._


class Application @Inject() (implicit system: ActorSystem, materializer: Materializer) extends Controller{
  val templateHandler = new TemplateHandler

  def index = Action {
    Ok(views.html.index("hej"))
  }

  def socket = WebSocket.accept[String, String] { request =>

//    def instantiate[T](clazz: java.lang.Class[T])(args:AnyRef*): T = {
//      val constructor = clazz.getConstructors()(0)
//      return constructor.newInstance(args:_*).asInstanceOf[T]
//    }
//
//    def getClass(s: String) = Class.forName(s"shared.$s")
//
//    val c = instantiate(getClass("Req"))(new java.lang.String("hej"), UUID.random())
////    val c1 = getClass("has").newInstance()
//
//    println(c)


    ActorFlow.actorRef(out => WebSocketActor.props(out))
  }

  /**
    * Excluded for now. Since element are needed at start of client application, the async nature of JS does not allow for sync reads.
    */
  //  val elemListHandler = new ElementListHandler
//  def getEntities = Action {
//    Ok(write[List[String]](elemListHandler.getEntities))
//  }
//
//  def getIntAttributes = Action {
//    Ok(write[List[String]](elemListHandler.getIntAttributes))
//  }
//
//  def getStringAttributes = Action {
//    Ok(write[List[String]](elemListHandler.getStringAttributes))
//  }
//
//  def getStatusValueAttributes = Action {
//    Ok(write[List[String]](elemListHandler.getStatusValueAttributes))
//  }

//  def template(templateNbr: Int) = Action {
//    Ok(templateHandler.getTemplate(templateNbr).getOrElse(NotFound))
//  }


}
