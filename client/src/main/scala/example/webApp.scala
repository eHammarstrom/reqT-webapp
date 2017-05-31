package example

import diode.Action
import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy
import example.Modal.ModalType
import scalacss.ScalaCssReact._
import scalacss.Defaults._
import scala.collection.immutable.Queue
import shared._

@JSExport
object webApp extends js.JSApp {

//  case class modelProps(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem])

  case class Props(proxy: ModelProxy[Tree])

  case class CachedModel(name: String, model: Tree, selected: Boolean, uUID: UUID)

  case class OpenModals(isModalOpen: Boolean = false, isNewModelModalOpen: Boolean = false)

  case class State(modalType: ModalType, treeItem: TreeItem,
                   dispatch: (Action => Callback) = null, path: Seq[String] = Seq(), elemToModal: Option[Elem] = None,
                   cachedModels: Queue[CachedModel] = Queue(CachedModel("untitled", emptyTree, selected = true, uUID = UUID.random())),
                   openModals: OpenModals = OpenModals(), saveModelType : String = "rec",
                   isMethodStarted: Boolean = false, scrollPosition: Double = 0, newModel: Tree = emptyTree, method: Seq[String] = Seq()) {
  }

  val emptyTree = Tree(Seq())

  def elemToTreeItem(elems: Seq[Elem]): TreeItem = {
    TreeItem("Model", UUID.model(), elems.map(elem => convert(elem)), None)
  }

  def convert(elem: Elem): TreeItem = elem match {
    case relation: Relation => TreeItem(relation.entity, relation.entity.uuid, relation.submodel.children.map(convert), Some(relation.link))
    case node: shared.Node => TreeItem(node, node.uuid, Seq(), None)
  }


  import org.scalajs.dom


  class Backend($: BackendScope[Props, State]) {

    def closeModal(e: ReactEvent): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isModalOpen = false)))

    def closeNewModelModal(): Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isNewModelModalOpen = false)))
    
    def saveScrollPosition(position: Double): Callback = {
      if($.accessDirect.state.scrollPosition != position)
        $.modState(_.copy(scrollPosition = position))
      else
        Callback()
    }


    def openModalWithContent(modalType: ModalType, treeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElemToAdd: Option[Elem]): Callback
    = $.modState(_.copy(modalType = modalType, treeItem = treeItem, openModals = OpenModals(isModalOpen = true), dispatch = newDispatch, path = newPath, elemToModal = newElemToAdd))

    def openNewModelModal(newSaveModelType: String, newModel: Tree): Callback = $.modState(_.copy(openModals = OpenModals(isNewModelModalOpen = true),
      saveModelType = newSaveModelType, newModel = newModel))

    val treeView = ReactComponentB[(ModelProxy[Tree], (ModalType, TreeItem, (Action => Callback), Seq[String], Option[Elem]) => Callback)]("treeView")
      .render(P => <.pre(
        ^.className := "zoomViewport",
        Styles.treeView,
        ^.border := "1px solid #ccc",
        ^.id := "treeView",
        <.div(
          ReactTreeView(
            root = elemToTreeItem(P.props._1.value.children),
            openByDefault = true,
            modelProxy = P.props._1,
            showSearchBox = true,
            setModalContent = P.props._2,
            saveScrollPosition = saveScrollPosition(_)
          ),
          <.strong(
            ^.id := "treeviewcontent"
          )
        )
      ))
      .build

    def setScroll(scrollPosition: Double): Callback = {
      var temp = document.getElementById("treeView").asInstanceOf[dom.html.Pre]
      Callback(temp.scrollTop = scrollPosition)
    }

    def getScroll: Callback = $.modState(_.copy(scrollPosition = document.getElementById("treeView").scrollTop))


    def saveModel(name: String, model: Tree, P: Props): Callback =
      $.modState(s => s.copy(cachedModels = s.cachedModels :+ CachedModel(name, model, selected = false, UUID.random())))

    def sendMethod(currentMethod: Seq[String]) = $.modState(_.copy(method = currentMethod, isMethodStarted = true))

    def methodDone = $.modState(_.copy(isMethodStarted = false))

    def render(P: Props, S: State) = {
      val sc = AppCircuit.connect(_.tree)


      <.div(
        Modal(S.openModals.isModalOpen, closeModal, S.modalType, S.treeItem, S.dispatch, S.path, S.elemToModal),
        NewModelModal(isOpen = S.openModals.isNewModelModalOpen, onClose = closeNewModelModal, saveModel = saveModel(_, _, P),
          S.newModel, S.saveModelType),
        ^.className := "container",
        ^.width := "100%",
        ^.height := "100%",
        ^.overflow := "hidden",
        ^.paddingRight := "5px",
        ^.paddingLeft := "5px",
        <.div(
          ^.className := "header",
          Header(P.proxy, openNewModelModal, sendMethod, getActiveModelName)
        ),
        <.div(
          ^.className := "col-1",
          ^.float := "left",
          ^.width := "29%",
          ^.height := "100%",
          ^.paddingRight := "9px",
          ElementList(),
          ReqTLog(P.proxy, openNewModelModal, () => S.method, S.isMethodStarted, methodDone)
        ),
        <.div(
          ^.className := "col-2",
          ^.width := "71%",
          ^.height := "100%",
          ^.float.left,
          cachedModels((P,S)),
//          CachedModels(P.proxy),
          sc(proxy => treeView((proxy, openModalWithContent)))
        )
      )
    }

    val cachedModels = ReactComponentB[(Props, State)]("cachedModelsComponent")
      .render($ => <.pre(
        ^.padding := "5px",
        ^.paddingRight := "5px",
        ^.height := "5%",
        ^.overflow := "hidden",
        ^.position.relative,
        <.button(
          ^.className := "glyphicon glyphicon-plus",
          ^.color := "green",
          ^.position.absolute,
          ^.top := "0%",
          ^.width := "5%",
          ^.height := "105%",
          ^.marginLeft := "-6px",
          ^.marginTop := "-2px",
          Styles.navBarButton,
          ^.outline := "none",
          ^.onClick --> openNewModelModal("save", $.props._1.proxy.value)
        ),
        <.div(
          ^.overflowX.auto,
          ^.left := "5%",
          ^.height := "91%",
          ^.overflowX.auto,
          ^.overflowY.hidden,
          ^.width := "95%",
          ^.position.absolute,
          <.div(
            ^.whiteSpace := "nowrap",
            ^.position.absolute,
            ^.className := "clickable-row",
              <.ul(
                ^.display.flex,
                ^.height := "0px",
                ^.className := "nav nav-pills",
                ^.listStyleType.none,
                $.props._2.cachedModels.reverse.map(s => listModels((s, $.props._1, $.props._2)))
              )
          )
        )
      )
      ).build

    val listModels = ReactComponentB[(CachedModel, Props, State)]("listElem")
      .render($ => <.li(
        ^.className := "navpill",
        ^.display := "inline",
        ^.whiteSpace := "nowrap",
        ^.position.relative,
        ^.marginLeft := "5px",
        ^.marginRight := "5px",
        ^.padding := "5px",
        ^.float.left,
        ^.overflow := "hidden",
        ^.borderRadius := "5px",
        ^.height := "30px",
        ^.top := "0px",
        ^.width := "200px",
        ^.background := "#CFEADD",
        ^.opacity := {if($.props._1.selected) "1" else "0.5"},

        <.span(
          ^.className := "col",
          ^.position.absolute,
          ^.width := "80%",
          ^.height := "30px",
          ^.paddingTop := "2px",
          ^.textAlign := "center",
          $.props._1.name
        ),
        ^.onClick --> setActiveModel($.props._1, $.props._2, $.props._3),

        <.button(
          ^.className := "col",
          ^.position.absolute,
          ^.width := "20%",
          ^.height := "30px",
          ^.left := "80%",
          ^.top := "0%",
          ^.paddingTop := "5px",
          Styles.removeButtonSimple,
          ^.outline := "none",
          ^.onClick ==> removeCachedModel($.props._1, $.props._2, $.props._3)
        )
      )).build

    def getActiveModelName: Option[CachedModel] = $.accessDirect.state.cachedModels.find(_.selected)


    def setActiveModel(cachedModel: CachedModel,P: Props, S: State): Callback = {
      updateActiveModel(cachedModel, P,S) >> P.proxy.dispatchCB(SetModel(cachedModel.model.children))
    }

    def updateActiveModel(cachedModel: CachedModel, P: Props, S: State): Callback = {
      val newModels: Queue[CachedModel] = S.cachedModels.map(model =>
        if (model.selected)
          model.copy(model = P.proxy.value, selected = model.uUID.equals(cachedModel.uUID))
        else
          model.copy(selected = model.uUID.equals(cachedModel.uUID))
      )
      $.modState(_.copy(cachedModels = newModels))
    }

    def removeCachedModel(modelToRemove: CachedModel, P: Props, S: State)(e: ReactEventI): Callback = {
      e.stopPropagation()
      val index = S.cachedModels.indexWhere(_.equals(modelToRemove))
      val beginning = S.cachedModels.take(index)
      val end = S.cachedModels.drop(index+1)

      $.modState(_.copy(cachedModels = beginning ++ end))
    }

  }
    val pageContent = ReactComponentB[Props]("Content")
      .initialState(State(modalType = Modal.EMPTY_MODAL, treeItem = null))
      .renderBackend[Backend]
        .componentWillReceiveProps( x => x.$.backend.setScroll(x.currentState.scrollPosition))
        .componentDidUpdate(x => {
        println("PageContent did Update")
      x.$.backend.setScroll(x.currentState.scrollPosition)
    })
      .build

    val dc = AppCircuit.connect(_.tree)

  def main(): Unit = {
    Styles.addToDocument()
    ReactDOM.render(dc(proxy => pageContent(Props(proxy))), document.getElementById("content"))
  }
}
