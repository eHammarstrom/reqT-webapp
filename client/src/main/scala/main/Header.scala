package main

import diode.react.ModelProxy
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent, ReactEvent, ReactEventFromInput}
import shared._
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import modals._
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{Blob, BlobPropertyBag, FileReader, UIEvent}
import org.scalajs.dom.{document, window}
import selects.TemplateSelect
import upickle.default.read

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
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

  case class Props(modelProxy: ModelProxy[Tree], openNewModelModal: (String, Tree) => Callback, sendMethod: Seq[String] => Callback, getCurrentModelName: Option[webApp.CachedModel])

  case class OpenModals(isDollarModalOpen: Boolean = false, isReleaseModalOpen: Boolean = false, isOrdinalModalOpen: Boolean = false, isCopyModalOpen: Boolean = false,
                        isHelpModalOpen: Boolean = false)

  val headerButtons = Seq("Import", "Export", "Copy Model", "Templates", "100$", "Ordinal", "Release", "Help")

  class Backend($: BackendScope[Props, State]) {

    def closeDollarModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isDollarModalOpen = false)))

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
        HundredDollarModal(isOpen = S.openModals.isDollarModalOpen, onClose = closeDollarModal, P.sendMethod, P.modelProxy.value.makeString),
        ReleaseModal(isOpen = S.openModals.isReleaseModalOpen, onClose = closeReleaseModal, P.sendMethod, P.modelProxy.value),
        OrdinalModal(isOpen = S.openModals.isOrdinalModalOpen, onClose = closeOrdinalModal, P.sendMethod, P.modelProxy.value),
        CopyModal(isOpen = S.openModals.isCopyModalOpen, onClose = closeCopyModal, P.modelProxy.value.makeString),
        HelpModal(isOpen = S.openModals.isHelpModalOpen, onClose = closeHelpModal),
        navigationBar((P, S))
      )
    }

    val navigationBar = ScalaComponent.builder[(Props, State)]("navigationBar")
      .render($ => <.nav(
        headerBarStyle.toTagMod,
        headerButtons.map(x => buttonComponent((x, $.props._1, $.props._2))).toTagMod
      )
      ).build


    val buttonComponent = ScalaComponent.builder[(String, Props, State)]("buttonComponent")
        .render($ => {
          val str = $.props._1
          val P = $.props._2
          val S = $.props._3

          str match {
            case "Import" =>
              <.label(
                headerButtonStyle.toTagMod,
                "Import",
                <.input(
                  ^.`type` := "file",
                  ^.display.none,
                  ^.accept := "text/plain, text/x-scala, .txt, .scala",
                  ^.onChange ==> importModel(P)
                )
              )
            case "Export" =>
              <.button(
                headerButtonStyle.toTagMod,
                "Export",
                ^.onClick --> Callback(downloadModel(P, S))
              )
            case "Copy Model" =>
              <.button(
                headerButtonStyle.toTagMod,
                "Copy Model",
                ^.onClick --> openCopyModal
              )
            case "Templates" =>
              TemplateSelect(P.openNewModelModal)
            case "100$" =>
              <.button(
                headerButtonStyle.toTagMod,
                "100$",
                ^.onClick --> openDollarModal
              )
            case "Release" =>
              <.button(
                headerButtonStyle.toTagMod,
                "Release",
                ^.onClick --> openReleaseModal
              )
            case "Ordinal" =>
              <.button(
                headerButtonStyle.toTagMod,
                "Ordinal Ranking",
                ^.onClick --> openOrdinalModal
              )
            case "Help" =>
              <.button(
                headerButtonStyle.toTagMod,
                ^.className := "glyphicon glyphicon-question-sign pull-right",
                ^.onClick --> openHelpModal
              )
            case _ =>
              <.button(
                str,
                headerButtonStyle.toTagMod,
                ^.onClick --> Callback()
              )
          }
        }
      )
      .build

    def parseModel(newModel: String, P: Props): Callback = {
      val getModelEndpoint = "/parsemodel"

      Ajax.post(url = getModelEndpoint, data = newModel.trim).onComplete {
        case Success(r) => Callback(println(r.responseText))
          P.openNewModelModal("imp", read[Model](r.responseText).tree).runNow()
        case Failure(e) => Callback(println(e.toString))
      }

      Callback() // FIXME: Handle Ajax.post.failed
    }

    def importModel(P: Props)(e: ReactEventFromInput): Callback = {
      val ftype = e.currentTarget.files.item(0).`type`
      if (ftype == "text/plain" || ftype == "text/x-scala") {
        var newModel = "newModel empty, shouldn't happen"
        val fileReader = new FileReader
        fileReader.readAsText(e.currentTarget.files.item(0), "UTF-8")

        fileReader.onload = (_: UIEvent) => {
          newModel = fileReader.result.asInstanceOf[String]
          parseModel(newModel.trim, P)
        }
        Callback(e.currentTarget.value = "")
      } else {
        Callback(window.alert("Invalid file type, only .txt and .scala is supported"))
      }
    }

    import org.scalajs.dom
    import scala.scalajs.js.timers._

    def downloadModel(P: Props, S: State): Unit = {
      val file = new Blob(js.Array(P.modelProxy.value.makeString), js.Dynamic.literal(`type` = "text/plain").asInstanceOf[BlobPropertyBag])
      val downloadElement = document.createElement("a")
      val tempURL = dom.URL.createObjectURL(file)
      downloadElement.setAttribute("href", tempURL)

      P.getCurrentModelName match {
        case Some(cachedModel) => downloadElement.setAttribute("download", s"${cachedModel.name}.scala")
        case None => downloadElement.setAttribute("download", "reqTModel.scala")
      }

      document.body.appendChild(downloadElement)
      downloadElement.asInstanceOf[dom.raw.HTMLBodyElement].click()
      setTimeout(1000) {
        document.body.removeChild(downloadElement)
        dom.URL.revokeObjectURL(tempURL)
      }
    }


  }

  val component = ScalaComponent.builder[Props]("Modal")
    .initialState(State())
    .renderBackend[Backend]
    .build


  def apply(modelProxy: ModelProxy[Tree], openNewModelModal: (String, Tree) => Callback, sendMethod: Seq[String] => Callback, getCurrentModelName: Option[webApp.CachedModel])
  = component(Props(modelProxy, openNewModelModal, sendMethod, getCurrentModelName))

}
