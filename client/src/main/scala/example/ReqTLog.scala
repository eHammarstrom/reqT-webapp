package example

import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw._
import shared.{Model, Tree}
import upickle.default.read

import scala.util.{Failure, Success}

/**
  * Created by johan on 5/26/17.
  */
object ReqTLog {

  case class State(websocket: Option[WebSocket], logLines: Vector[String], message: String, isMethodStarted: Boolean = false, waitingForModel: Boolean = false){
    def log(line: String): State = copy(logLines = logLines :+ line)
  }

  case class Props(proxy: ModelProxy[Tree], stateSaveTree: Tree => Unit, openNewModelModal: (String, Tree) => Callback, getMethod: () => Seq[String], runMethod: Boolean)

  class Backend($: BackendScope[Props, State]) {


    def render(P: Props, S: State) = {

      def setMethodStarted: Callback = $.modState(_.copy(isMethodStarted = true))

      val sendVerify: Option[Callback] = {
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessage(websocket, P.proxy.value.makeString.replaceAll("\n", ""))
      }


      def sendPrepMessage(msgs: Seq[String]): Option[Seq[Callback]] = {
        for (websocket <- S.websocket if P.proxy.value.toString.nonEmpty)
          yield sendMessages(websocket, msgs) :+ setMethodStarted
      }


      val send: Option[Callback] ={
        for (websocket <- S.websocket if S.message.nonEmpty)
          yield sendMessage(websocket, S.message)
      }

      def handleKeyDown(event: ReactKeyboardEventI): Option[Callback] = {
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
            "Verify Model",
            ^.onClick -->? sendVerify
          )
        ),
        log(S.logLines)
      )
    }

    val log = ReactComponentB[Vector[String]]("log")
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
          $.props.map(<.p(_)))
      )
      .componentDidUpdate(_ => updateScroll)
      .componentDidMount(_ => Callback({
      var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
      reqtLog.setAttribute("style", "user-select:text;" + reqtLog.style.cssText)
    }))
      .build

    def updateScroll: Callback = {
      Callback({
        var reqtLog = document.getElementById("reqTLog").asInstanceOf[dom.html.Pre]
        reqtLog.scrollTop = reqtLog.scrollHeight
      })
    }


    def onChange(event: ReactEventI): Callback = {
      val newMessage = event.target.value
      $.modState(_.copy(message = newMessage))
    }




    def sendMessage(websocket: WebSocket, msg: String): Callback = {
      def send(msg : String) = Callback(websocket.send(msg))
      def updateState = $.modState(s => s.log(s"Sent: \n$msg").copy(message = ""))
      def setStateToCatchModel = $.modState(_.copy(waitingForModel = true))

      if(msg.startsWith("get "))
        setStateToCatchModel >> send(msg.replaceFirst("get ", "")) >> updateState
      else
        send(msg) >> updateState
    }

    def sendMessages(websocket: WebSocket, msg: Seq[String]): Seq[Callback] = msg.map(sendMessage(websocket,_))

    def receiveModel(S: State, P: Props, tree: Tree) = {
      //      P.stateSaveTree(tree)
      if (S.isMethodStarted || S.waitingForModel){
        P.openNewModelModal("rec", tree)
        $.accessDirect.modState(_.copy(isMethodStarted = false))
      }
    }

    def start(P: Props): Callback = {

      // This will establish the connection and return the WebSocket
      def connect = CallbackTo[WebSocket] {

        // Get direct access so WebSockets API can modify state directly.
        val direct = $.accessDirect

        def onopen(event: Event): Unit = {
          direct.modState(_.log("Connected."))
        }


        def onmessage(event: MessageEvent): Unit = {
          if(event.data.toString.startsWith("{")){
            val tree = read[Model](event.data.toString).tree
            receiveModel(direct.state, P, tree)
          } else {
            direct.modState(_.log(s"${event.data.toString}"))
          }
          direct.modState(_.copy(waitingForModel = false))
        }

        def onerror(event: ErrorEvent): Unit = {
          direct.modState(_.log(s"Error: ${event.message}"))
        }

        def onclose(event: CloseEvent): Unit = {
          direct.modState(_.copy(websocket = None).log(s"Closed: ${event.reason}"))
        }


        val url = "ws://127.0.0.1:9000/socket"


        val websocket = new WebSocket(url)
        websocket.onopen = onopen _
        websocket.onclose = onclose _
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



  val component = ReactComponentB[Props]("ReqTLog")
    .initialState(State(None, Vector.empty, message = ""))
    .renderBackend[Backend]
    .componentDidMount(x =>  x.backend.start(x.props))
    .componentWillReceiveProps(
      x => if (x.nextProps.runMethod){
        x.$.modState(_.copy(isMethodStarted = true))
        Callback(x.nextProps.getMethod().map(s => x.$.backend.sendMessage(x.$.state.websocket.get,s).runNow()))
      }
      else Callback()
    )
    .componentWillUnmount(_.backend.end)
    .build


  def apply(proxy: ModelProxy[Tree], stateSaveTree: Tree => Unit, openNewModelModal: (String, Tree) => Callback, getMethod: () => Seq[String], runMethod: Boolean)
  = component.set()(Props(proxy, stateSaveTree, openNewModelModal, getMethod, runMethod))

}
