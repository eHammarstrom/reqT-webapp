package example

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactEventI}
import shared._
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, UIEvent}
import org.scalajs.dom.{document, window}
import upickle.default.read
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.URIUtils.encodeURI
import scala.util.{Failure, Success}

/**
  * Created by phiped on 5/26/17.
  */
object Header {

  val headerButtonStyle = Seq(
    ^.className := "btn btn-default navbar-btn",
    ^.margin := "5px",
    ^.padding := "10px"
  )

  val headerBarStyle = Seq(
    ^.paddingLeft := "15px",
    ^.paddingRight := "15px",
    ^.className := "navbar navbar-default navbar-static-bottom"
  )

  case class State(openModals: OpenModals = OpenModals())
  case class Props(modelProxy: ModelProxy[Tree], openNewModelModal: (String, Tree) => Callback)

  case class OpenModals(isDollarModalOpen: Boolean = false, isReleaseModalOpen: Boolean = false, isOrdinalModalOpen: Boolean = false, isCopyModalOpen: Boolean = false,
                        isHelpModalOpen: Boolean = false)

  val headerButtons = Seq("Import", "Export", "Copy Model", "Templates", "100$", "Ordinal", "Release", "Help")

  class Backend($: BackendScope[Props, State]) {

    def closeDollarModal(): Callback =  $.modState(S => S.copy(openModals = S.openModals.copy(isDollarModalOpen = false)))
    def closeReleaseModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isReleaseModalOpen = false)))
    def closeOrdinalModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isOrdinalModalOpen = false)))
    def closeCopyModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isCopyModalOpen = false)))
    def closeHelpModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isHelpModalOpen = false)))

    def openDollarModal: Callback = $.modState(_.copy(openModals = OpenModals(isDollarModalOpen = true)))
    def openOrdinalModal: Callback = $.modState(_.copy(openModals = OpenModals(isOrdinalModalOpen = true)))
    def openReleaseModal: Callback = $.modState(_.copy(openModals = OpenModals(isReleaseModalOpen = true)))
    def openCopyModal: Callback = $.modState(_.copy(openModals = OpenModals(isCopyModalOpen = true)))
    def openHelpModal: Callback = $.modState(_.copy(openModals = OpenModals(isHelpModalOpen = true)))


    def render(P: Props, S: State) = {
      <.div(
        HundredDollarModal(isOpen = S.openModals.isDollarModalOpen, onClose = closeDollarModal, sendPrepMessage),
        ReleaseModal(isOpen = S.openModals.isReleaseModalOpen, onClose = closeReleaseModal, sendPrepMessage, P.modelProxy.value),
        OrdinalModal(isOpen = S.openModals.isOrdinalModalOpen, onClose = closeOrdinalModal, sendPrepMessage, P.modelProxy.value),
        CopyModal(isOpen = S.openModals.isCopyModalOpen, onClose = closeCopyModal, P.modelProxy.value.makeString),
        navigationBar((P,S))
      )
    }

    def sendPrepMessage(prepMessage: String => Seq[String]): Option[Seq[Callback]] = {
        Some(Seq(Callback(println("SENDMSG"))))
    }

    val navigationBar = ReactComponentB[(Props, State)]("navigationBar")
      .render($ => <.nav(
        headerBarStyle,
        headerButtons.map(x => buttonComponent((x, $.props._1, $.props._2)))
      )
      ).build


    val buttonComponent = ReactComponentB[(String, Props, State)]("buttonComponent")
      .render($ =>
        $.props._1 match {
          case "Import" => <.label(
            headerButtonStyle,
            "Import",
            <.input(
              ^.`type`:="file",
              ^.display.none,
              ^.accept := "text/plain, .txt",
              ^.onChange ==> importModel($.props._2.modelProxy.dispatchCB)
            )
          )
          case "Export" =>
            <.button(
              headerButtonStyle,
              "Export",
              ^.onClick --> Callback(downloadModel($.props._2, $.props._3))
            )
          case "Copy Model" =>
            <.button(
              headerButtonStyle,
              "Copy Model",
              ^.onClick --> openCopyModal
            )
          case "Templates" => TemplateSelect(setTemplate, $.props._2.openNewModelModal)
          case "100$" =>
            <.button(
              headerButtonStyle,
              "100$",
              ^.onClick --> openDollarModal
            )
          case "Release" =>
            <.button(
              headerButtonStyle,
              "Release",
              ^.onClick --> openReleaseModal
            )
          case "Ordinal" =>
            <.button(
              headerButtonStyle,
              "Ordinal Ranking",
              ^.onClick --> openOrdinalModal
            )
          case "Help" =>
            <.button(
              headerButtonStyle,
              ^.className := "glyphicon glyphicon-question-sign pull-right",
              ^.onClick --> openHelpModal
            )
          case _ => <.button(
            $.props._1,
            headerButtonStyle,
            ^.onClick --> Callback()
          )
        }).build

    def setTemplate(child: Seq[Elem]): Callback = Callback(println(child.toString()))

    // Klara inte specialtecken i strängar  ------------------------------------------------------------------!!!!!

    def parseModel(newModel: String, dispatch: Action => Callback): Callback = {
      Ajax.get("/getmodelfromstring/" + encodeURI(newModel.trim.replaceAll(" +", " "))).onComplete{
        case Success(r) => Callback(println(r.responseText))
//          openNewModelModal("imp", SetModel(read[Model](r.responseText).tree)).runNow
        case Failure(e) => Callback(println(e.toString))
      }
      Callback()
    }


    def importModel(dispatch: Action => Callback)(e: ReactEventI): Callback = {
      if(e.currentTarget.files.item(0).`type` == "text/plain") {
        var newModel = "newModel empty, shouldn't happen"
        val fileReader = new FileReader
        fileReader.readAsText(e.currentTarget.files.item(0), "UTF-8")

        fileReader.onload = (_: UIEvent) => {
          newModel = fileReader.result.asInstanceOf[String]
          parseModel(newModel.replace("\n", "").trim, dispatch)
        }
        Callback(e.currentTarget.value = "")
      } else {
        Callback(window.alert("Invalid file type, only .txt is supported"))
      }
    }

    import org.scalajs.dom
    import scala.scalajs.js.timers._

    def downloadModel(P: Props, S: State): Unit = {
      var file = new Blob(js.Array(P.modelProxy.value.makeString), js.Dynamic.literal(`type` = "text/plain").asInstanceOf[BlobPropertyBag])
      val a = document.createElement("a")
      var tempURL = dom.URL.createObjectURL(file)
      a.setAttribute("href", tempURL)

//      if(S.cachedModels.exists(_.selected)){
//        a.setAttribute("download", s"${S.cachedModels.find(_.selected).get.name}.txt")
//      } else {
//        a.setAttribute("download", "reqTModel.txt")
//      }

      a.setAttribute("download", "reqTModel.txt")

      document.body.appendChild(a)
      a.asInstanceOf[dom.raw.HTMLBodyElement].click()
      setTimeout(1000) {
        document.body.removeChild(a)
        dom.URL.revokeObjectURL(tempURL)
      }
    }


  }

  val component = ReactComponentB[Props]("Modal")
    .initialState(State())
    .renderBackend[Backend]
    .build


  def apply(modelProxy: ModelProxy[Tree], openCachedModelModal: (String, Tree) => Callback)
  = component.set()(Props(modelProxy, openCachedModelModal))

}