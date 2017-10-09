package main

import java.util.concurrent.TimeUnit

import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw._
import shared.{Model, Tree}
import upickle.default.read

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}
import shared.Config

object ReqTLog {

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, waitingForModel: Boolean = false, isMethodRunning: Boolean = false) {
    def log(line: String): State = copy(logLines = logLines :+ line)
  }

  case class Props(proxy: ModelProxy[Tree], openNewModelModal: (String, Tree) => Callback, getMethod: () => Seq[String], runMethod: Boolean, methodDone: Callback)

  class Backend($: BackendScope[Props, State]) {

    def render(P: Props, S: State) = {

      val sendVerify: Option[Callback] = {
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, P.proxy.value.makeString.replaceAll("\n", ""))
      }

      val send: Option[Callback] = {
        for (websocket <- S.websocket if S.message.nonEmpty)
          yield sendMessage(websocket, S.message)
      }

      def handleKeyDown(event: ReactKeyboardEventFromInput): Option[Callback] = {
        if (event.nativeEvent.keyCode == KeyCode.Enter)
          send
        else
          None
      }

      <.pre(
        ^.height := "45%",
        ^.overflow.hidden,
        <.div(
          <.input(
            ^.className := "form-control",
            ^.marginBottom := "5px",
            ^.onChange ==> onChange,
            ^.value := S.message,
            ^.onKeyDown ==>? handleKeyDown
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.disabled := send.isEmpty,
            ^.onClick -->? send,
            "Send"),
          <.button(
            ^.disabled := sendVerify.isEmpty,
            ^.className := "btn btn-default",
            "Send Model",
            ^.onClick -->? sendVerify
          ),
          <.button(
            ^.className := "btn btn-default pull-right",
            ^.onClick --> restartReqT(P),
            "Restart reqT"
          )
        ),
        log(S.logLines)
      )
    }

    val log = ScalaComponent.builder[Vector[String]]("log")
      .render($ =>
        <.pre(
          ^.className := "form-control",
          ^.id := "reqTLog",
          ^.width := "auto",
          ^.height := "80%",
          ^.marginTop := "5px",
          ^.overflowY.auto,
          ^.overflowX.hidden,
          ^.whiteSpace.`pre-line`,
          ^.wordBreak := "break-word",
          $.props.map(<.p(_)).toTagMod
        )
      )
      .componentDidUpdate(_ => updateScroll)
      .componentDidMount(_ => Callback({
        var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
        reqtLog.setAttribute("style", "user-select:text;" + reqtLog.style.cssText)
      }))
      .build

    def restartReqT(P: Props): Callback = {
      start(P).delay(FiniteDuration(1, TimeUnit.SECONDS)).runNow()
      end
    }

    def updateScroll: Callback = {
      Callback({
        var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
        reqtLog.scrollTop = reqtLog.scrollHeight
      })
    }

    def onChange(event: ReactEventFromInput): Callback = {
      val newMessage = event.target.value
      $.modState(_.copy(message = newMessage))
    }

    def sendMessage(websocket: WebSocket, msg: String): Callback = {
      def send(msg: String) = Callback(websocket.send(msg))

      def updateState = $.modState(s => s.log(s"Sent: \n$msg").copy(message = ""))

      def setStateToCatchModel = $.modState(_.copy(waitingForModel = true))

      if (msg.startsWith("get "))
        setStateToCatchModel >> send(msg.replaceFirst("get ", "")) >> updateState
      else
        send(msg) >> updateState
    }

    def sendMessages(websocket: WebSocket, msg: Seq[String]): Unit = {
      msg.foreach(sendMessage(websocket, _).runNow())
      $.modState(_.copy(isMethodRunning = true)).runNow()
    }

    def receiveModel(S: State, P: Props, tree: Tree): Unit = {
      if (S.isMethodRunning || S.waitingForModel) {
        P.openNewModelModal("rec", tree).runNow()
        $.modState(_.copy(isMethodRunning = false)).runNow()
      }
    }

    def start(P: Props): Callback = {
      // This will establish the connection and return the WebSocket
      def connect = CallbackTo[WebSocket] {
        def onopen(event: Event): Unit = {
          $.modState(_.log("Connected.")).runNow()
        }

        def onmessage(event: MessageEvent): Unit = {
          if (event.data.toString.startsWith("{")) {
            val tree = read[Model](event.data.toString).tree
            receiveModel($.state.runNow(), P, tree)
          } else {
            $.modState(_.log(s"${event.data.toString}")).runNow()
          }
          $.modState(_.copy(waitingForModel = false)).runNow()
        }

        def onerror(event: ErrorEvent): Unit = {
          $.modState(_.log(s"Error: ${event.message}")).runNow()
        }

        def onclose(webSocket: WebSocket)(event: CloseEvent): Unit = {
          webSocket.close()
          $.modState(_.copy(websocket = None).log(s"Closed: ${event.reason}")).runNow()
        }

        val url = Config().socketURL

        val websocket = new WebSocket(url)
        websocket.onopen = onopen _
        websocket.onclose = onclose(websocket: WebSocket) _
        websocket.onmessage = onmessage _
        websocket.onerror = onerror _
        websocket
      }

      // Here use attemptTry to catch any exceptions in connect.
      connect.attemptTry.flatMap {
        case Success(websocket) => $.modState(_.log("Connecting...").copy(websocket = Some(websocket)))
        case Failure(error) => $.modState(_.log(error.toString))
      }
    }

    def end: Callback = {
      def closeWebSocket = $.state.map(_.websocket.foreach(_.close()))

      def clearWebSocket = $.modState(_.copy(websocket = None))

      closeWebSocket >> clearWebSocket
    }

  }

  val component = ScalaComponent.builder[Props]("ReqTLog")
    .initialState(State(None, Vector.empty, message = ""))
    .renderBackend[Backend]
    .componentDidMount(x => x.backend.start(x.props))
    .componentWillReceiveProps(
      x => if (x.nextProps.runMethod) {
        x.backend.sendMessages(x.state.websocket.get, x.nextProps.getMethod())
        x.nextProps.methodDone
      }
      else Callback()
    )
    .componentWillUnmount(_.backend.end)
    .build

  def apply(proxy: ModelProxy[Tree], openNewModelModal: (String, Tree) => Callback, getMethod: () => Seq[String], runMethod: Boolean, methodDone: Callback) =
    component(Props(proxy, openNewModelModal, getMethod, runMethod, methodDone))

}
