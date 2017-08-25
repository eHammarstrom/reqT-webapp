package controllers

import akka.actor._
import shared._

import upickle.default._


object WebSocketActor {
  def props(out: ActorRef) = Props(new WebSocketActor(out))
}


class WebSocketActor(out: ActorRef) extends Actor {
  val templateHandler = new TemplateHandler
  val parser = new ExprParser


  val sysRuntime = Runtime.getRuntime
  val reqTprocess = sysRuntime.exec("java -jar reqT.jar serv")
  val (reqTis, reqTos) = (reqTprocess.getInputStream, reqTprocess.getOutputStream)
  val buf = new Array[Byte](10000)

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def findEndOfModel(model: List[Char]): Int = {
    def go(cs: List[Char], level: Int): Int = cs match {
      case ')' :: _ if level == 1 => 1
      case ')' :: xs => go(xs, level - 1) + 1
      case '(' :: xs => go(xs, level + 1) + 1
      case _ :: xs => go(xs, level) + 1
      case Nil if level != 0 => Int.MinValue
    }

    go(model, 0)
  }

  def trimModel(model: String): Option[String] = {
    val end = findEndOfModel(model.toList)
    end match {
      case x if x < 0 => None
      case _ => Some(model.take(end))
    }
  }

  private def sendResponse(resp: String) = {
    val response = s"Answer:\n$resp\n"

    """= *Model[(].*[)]""".r.findFirstMatchIn(response.replaceAll("\n", " ")) match {

      case _ if response.contains("dollarMethod") | response.contains("releaseMethod") | response.contains("val ordinalMethod") =>
        out ! response

      case Some(m) =>
        trimModel(m.matched.drop(m.matched.indexOf("Model")).replaceAll(" +", " ")) match {
          case Some(model) => out ! write[Model](parser.parseAll(parser.Model, model).get)
          case _ => Unit
        }
        out ! response

      case None => out ! response

    }
  }
  @volatile var stopReadThread = false

  val readThread = new Thread(new Runnable {
    override def run() = {
      while (reqTprocess.isAlive && !stopReadThread) {
        var releasePlanDone = true
        var counter = 0
        if (reqTis.available() > 0) {
          val nbrOfReadBytes = reqTis.read(buf, 0, 10000)
          val response = buf.take(nbrOfReadBytes).map(_.toChar).mkString
          if(response.contains("releaseMethod")) releasePlanDone = false
          if(response.contains("solution")) releasePlanDone = true
          sendResponse(response.replaceAll("<!-- reqT server ready for input -->", "").replaceAll("[\r\n]+", "\n"))

        } else {
          Thread.sleep(500)
          if(!releasePlanDone){
            counter += 1
            if(counter > 5){
              counter = 0
              sendResponse("reqT is still evaluating")
            }
          }
        }
      }
    }
  })
  readThread.start()

  override def postStop() = {
    reqTprocess.destroy()
    stopReadThread = true
  }


  def receive = {
    case message: String if message.contains("val ordinalMethod") =>
      val newmessage = message.replaceAll("ä","a").replaceAll("å","a").replaceAll("ö","o")
      reqTos.write((newmessage + ".replace(\";\", \"\\n\")" + "\n").getBytes("UTF-8"))
      reqTos.flush()

    case message: String =>
      reqTos.write((message + "\n").getBytes("UTF-8"))
      reqTos.flush()

  }
}
