package example

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react.CompScope._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^.{^, _}

import scalacss.ScalaCssReact._
import scala.scalajs.js
import shared._
import diode.NoAction
import org.scalajs.dom.document

case class TreeItem(var item: Any, var uuid: UUID, var children: Seq[TreeItem], var link: Option[RelationType]) {
  def linkToString: String = link match {
    case Some(relationType) => relationType.getType
    case None => ""
  }

  def itemToString: String = item.toString.replaceAll("\"", "")

  def entityToString: String =
    item match {
      case i :Entity => i.getType
      case i: IntAttribute => i.getType
      case i: StringAttribute   => i.getType
      case "Model" => item.toString
    }

  def contentToString: String =
    item match {
      case i :Entity => i.getID
      case i: IntAttribute => i.getValue.toString
      case i: StringAttribute   => i.getValue
      case "Model" => item.toString
    }
  
  def apply(item: Any, uuid: UUID, children: Seq[TreeItem]): TreeItem = this (item, uuid, Seq())

}

object ReactTreeView {
  case class Tuple(var uuid: UUID, collapsed: Boolean)
  case class AddTuple(tuple: Tuple) extends Action
  case class RemoveTuple(tuple: Tuple) extends Action
  case class ToggleCollapsed(uuid: UUID) extends Action

  trait Style {
    def reactTreeView = Seq[TagMod]()

    def treeGroup = Seq(^.margin := "5px", ^.padding := "0 0 0 40px")

    def treeItem = Seq(
      ^.listStyleType := "none",
      ^.marginTop := "10px",
      ^.paddingLeft := "20px"
    )

    def treeItemDiv = Seq(
      ^.boxShadow := "5px 6px 12px 0px rgba(0,0,0,0.2)",
      ^.overflow.visible,
      ^.position.relative,
      ^.borderRadius := "5px",
      ^.padding := "5px",
      ^.marginBottom := "10px",
      ^.marginRight := "0",
      ^.marginLeft := "0",
      ^.width := "500px",
      ^.key := "toggle",
      ^.className := "container",
      ^.draggable := true
    )

    def treeItemIdDiv = Seq(
      ^.className := "row",
      ^.overflow.hidden,
      ^.unselectable := "true",
      ^.position := "absolute",
      ^.left := "7%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.width := "440px",
      ^.paddingLeft := "0px",
      ^.paddingRight := "0px",
      ^.fontSize := "medium"
    )

    def treeItemBefore = Seq(
      ^.position := "absolute",
      ^.height := "100.%%",
      ^.left := "2.%%",
      ^.display := "inline-block",
      ^.fontSize := "14px",
      ^.color := "grey",
      ^.textAlign := "center",
      ^.width := "11px"
    )

    def attributeDiv = Seq(
      ^.className := "col",
      ^.height := "100%",
      ^.width := "40%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.position := "absolute",
      ^.left := "0%",
      ^.paddingTop := "3%",
      ^.paddingLeft := "3%"
    )

    def elemDiv1 = Seq(
      ^.fontStyle.oblique,
      ^.className := "col",
      ^.height := "100%",
      ^.width := "30%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.position := "absolute",
      ^.left := "0%",
      ^.paddingTop := "3%",
      ^.paddingLeft := "3%"
    )

    def elemDiv2 = Seq(
      ^.width := "0px",
      ^.height := "100%",
      ^.float.left,
      ^.border := "1px inset",
      ^.left := "30%",
      ^.position := "absolute",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.opacity := "0.5"
    )

    def elemDiv3 = Seq(
      ^.className := "col",
      ^.height := "100%",
      ^.width := "70%",
      ^.top := "0px",
      ^.bottom := "0px",
      ^.paddingLeft := "3%",
      ^.position := "absolute",
      ^.left := "30%",
      ^.overflow.hidden,
      ^.textAlign.justify,
      ^.wordWrap.`break-word`,
      ^.fontSize.small
    )

    def treeItemHasChildrenClosed = Seq(^.contentStyle := "▶")

    def treeItemHasChildrenOpened = Seq(^.contentStyle := "▼")
  }

  def getRelationType(treeItem: TreeItem): Option[String] = {
    treeItem.item match {
      case "Model" => None
      case elem : Elem  => if (elem.hasRelation) Some(treeItem.linkToString) else None
    }
  }

  type NodeC = DuringCallbackU[NodeProps, NodeState, NodeBackend]

  case class State(filterText: String,
                   filterMode: Boolean,
                   selectedNode: js.UndefOr[NodeC],
                   dragOverNode: js.UndefOr[NodeC],
                   scrollPosition: Double,
                   collapseList: Seq[Tuple] = Seq[Tuple](),
                   openModals: OpenModals = OpenModals(),
                   modelProps: ModelProps = ModelProps()
                  )

  case class Props(root: TreeItem,
                   open: Boolean,
                   onItemSelect: js.UndefOr[(String, String, Int) => Callback],
                   showSearchBox: Boolean,
                   style: Style,
                   modelProxy: ModelProxy[Tree],
                   saveScrollPosition: Double => Callback

                  )

  case class ModelProps(treeItem: TreeItem = null, dispatch: (Action => Callback) = null, path: Seq[String] = Seq(), elemToAdd: Option[Elem] = None)

  case class OpenModals(isAddElemModalOpen: Boolean = false, isEditModalOpen: Boolean = false, isDeleteModalOpen: Boolean = false)


  class Backend($: BackendScope[Props, State]) {

    def closeDeleteModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isDeleteModalOpen = false)))
    def closeEditModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isEditModalOpen = false)))
    def closeAddElemModal: Callback = $.modState(S => S.copy(openModals = S.openModals.copy(isAddElemModalOpen = false)))

    def openDeleteModal(newTreeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String]) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isDeleteModalOpen = true),
        modelProps = ModelProps(newTreeItem, newDispatch, newPath, None)))

    def openEditModal(newTreeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String]) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isEditModalOpen = true),
        modelProps = ModelProps(newTreeItem, newDispatch, newPath, None)))

    def openAddElemModal(newTreeItem: TreeItem, newDispatch: (Action => Callback), newPath: Seq[String], newElem: Option[Elem]) = $.modState(S =>
      S.copy(openModals = S.openModals.copy(isAddElemModalOpen = true),
        modelProps = ModelProps(newTreeItem, newDispatch, newPath, newElem)))

    def saveScrollPosition(position: Double): Callback =  $.modState(s => s.copy(scrollPosition = position))

    def onNodeSelect(P: Props)(selected: NodeC): Callback = {

      val removeSelection: Callback =
        $.state.flatMap(
          _.selectedNode
            .filterNot(_ == selected)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(selected = false)))
        )

      val updateThis: Callback =
        $.modState(_.copy(selectedNode = selected, filterMode = false))

      val setSelection: Callback =
        selected.modState(_.copy(selected = true))

      removeSelection >> updateThis >> setSelection
    }

    def onNodeDrag(P: Props)(draggedOver: NodeC): Callback = {
      val removeCurrent: Callback =
        $.state.flatMap(
          _.dragOverNode
            .filterNot(_ == draggedOver)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(draggedOver = false)))
        )

      val updateThis: Callback =
        $.modState(_.copy(dragOverNode = draggedOver, filterMode = false))

      val setCurrent: Callback =
        draggedOver.modState(_.copy(draggedOver = true))

      removeCurrent >> updateThis >> setCurrent
    }

    def onNodeDrop(P: Props)(draggedOver: NodeC): Callback = {
      val removeCurrent: Callback =
        $.state.flatMap(
          _.dragOverNode
            .filterNot(_ == draggedOver)
            .filter(_.isMounted())
            .fold(Callback.empty)(_.modState(_.copy(draggedOver = false)))
        )

      val updateThis: Callback =
        $.modState(_.copy(dragOverNode = draggedOver, filterMode = false))

      val setRemove: Callback =
        draggedOver.modState(_.copy(draggedOver = false))

      removeCurrent >> setRemove
    }

    def onTextChange(text: String): Callback =
      $.modState(_.copy(filterText = text, filterMode = true))

    def render(P: Props, S: State) =
      <.div(
        DeleteModal(S.openModals.isDeleteModalOpen, closeDeleteModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path),
        EditModal(S.openModals.isEditModalOpen, closeEditModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path),
        AddElemModal(S.openModals.isAddElemModalOpen, closeAddElemModal, S.modelProps.treeItem, S.modelProps.dispatch, S.modelProps.path, S.modelProps.elemToAdd),
        P.style.reactTreeView)(
          P.showSearchBox ?= ReactSearchBox(onTextChange = onTextChange),
          cc(proxy => TreeNode.withKey("root")(
            NodeProps(
              root = P.root,
              open = P.open,
              onNodeSelect = onNodeSelect(P),
              onNodeDrag = onNodeDrag(P),
              onNodeDrop = onNodeDrop(P),
              filterText = S.filterText,
              style = P.style,
              filterMode = S.filterMode,
              modelProxy = P.modelProxy,
              openDeleteModal = openDeleteModal,
              openEditModal = openEditModal,
              openAddElemModal = openAddElemModal,
              saveScrollPosition = P.saveScrollPosition,
              collapseProxy = proxy
            )
          )
          )
      )
  }

  case class NodeBackend($: BackendScope[NodeProps, NodeState]) {
    import upickle.default.read

    def dragStart(P: NodeProps)(event: ReactDragEvent): Callback = {
      val path = if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid.toString

        event.dataTransfer.effectAllowed = "move"
        event.dataTransfer.setData("existing", "true")
        event.dataTransfer.setData("path", path)
        Callback()
    }

    //def toggleOpen(P: NodeProps): NodeProps = P.copy(open = !P.open)

    def onTreeMenuToggle(P: NodeProps)(e: ReactEventH): Callback =
      P.collapseProxy.dispatchCB(ToggleCollapsed(P.root.uuid)) >> P.onNodeSelect($.asInstanceOf[NodeC]) >> childrenFromProps(P) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemSelect(P: NodeProps)(e: ReactEventH): Callback =
      P.onNodeSelect($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemDragOver(P: NodeProps)(e: ReactEventH): Callback =
      P.onNodeDrag($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def onItemDrop(P: NodeProps)(e: ReactDragEvent): Callback =
      P.onNodeDrop($.asInstanceOf[NodeC]) >> e.preventDefaultCB >> e.stopPropagationCB

    def childrenFromProps(P: NodeProps): CallbackTo[Option[Unit]] ={
      $.modState(S => S.copy(children = if (S.children.isEmpty){
        P.root.children
      }
      else Nil))
        .when(P.root.children.nonEmpty)
    }


    def matchesFilterText(filterText: String, data: TreeItem): Boolean = {
      def trim(data : String): String = data.toString.replaceAll("""(UUID\([^\)]*\))|TreeItem|List|None|[\(\),"]|Some"""," ").trim.replaceAll(" +", " ")

        def matchesType(treeItem: TreeItem): Boolean =
          treeItem.item match {
            case _: Entity => filterText.toLowerCase.contains("entity") || filterText.toLowerCase.contains("entities")
            case _: IntAttribute => filterText.toLowerCase.contains("attribute") && !filterText.toLowerCase.contains("stringattribute")
            case _: StringAttribute => filterText.toLowerCase.contains("attribute") && !filterText.toLowerCase.contains("intattribute")
            case _ => false
          }

        def matches(treeItem: TreeItem): Boolean = trim(treeItem.toString).toLowerCase.contains(filterText.toLowerCase)

        def relationTypeMatches(treeItem: TreeItem): Boolean = trim(treeItem.linkToString).toLowerCase.contains(filterText.toLowerCase)

        def loop(data: Seq[TreeItem]): Boolean =
          data.view.exists(
            item => matches(item) || loop(item.children) || relationTypeMatches(item) || matchesType(item)
          )

      (matches(data) || loop(data.children) || relationTypeMatches(data) || matchesType(data)) && (data.item != "Model")
    }

    def onDrop(P: NodeProps)(event: ReactDragEvent): Callback = {

      val pathFrom = event.dataTransfer.getData("path").split("/")
      val pathTo = (if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid).split("/")

      val dispatch: Action => Callback = P.modelProxy.dispatchCB

      val ctrlHeld = event.ctrlKey
      val notExistingElem = event.dataTransfer.getData("existing") == "false"
      val isModel = P.root.item.toString == "Model"
      val dropOnSelf = pathFrom.diff(pathTo).isEmpty
      val dropOnItsChildren = pathFrom.init.corresponds(pathTo)(_ == _)
      val droppingAttribute = event.dataTransfer.getData("type") == "stringAttr" || event.dataTransfer.getData("type") == "intAttr"
      var isAttribute = false

      val collapsed = P.collapseProxy.value.find(_.uuid == P.root.uuid) match {
        case Some(tuple) =>
          tuple.collapsed
        case None =>
          AddTuple(Tuple(P.root.uuid, collapsed = false))
          false
      }

      if(collapsed) {
        P.collapseProxy.dispatchCB(ToggleCollapsed(P.root.uuid)).runNow()
      }

      if (!isModel)
        isAttribute = P.root.item.asInstanceOf[Elem].isAttribute

      def getElem(event: ReactDragEvent): Elem = event.dataTransfer.getData("type") match {
        case "entity" => read[Entity](event.dataTransfer.getData("elem"))
        case "stringAttr" => read[StringAttribute](event.dataTransfer.getData("elem"))
        case "intAttr" => read[IntAttribute](event.dataTransfer.getData("elem"))
      }


      if (ctrlHeld)
        dispatch(CopyElem(pathFrom, pathTo, RelationType("has")))
      else if (isModel && droppingAttribute || isAttribute || dropOnSelf || dropOnItsChildren) {
        dispatch(NoAction)
      } else if (notExistingElem) {
        getElem(event) match {
          case entity: Entity => P.openAddElemModal(P.root, dispatch, pathTo, Some(entity))
          case intAttr: IntAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(intAttr))
          case stringAttr: StringAttribute => P.openAddElemModal(P.root, dispatch, pathTo, Some(stringAttr))
          case _ => Callback()
        }
      } else {
        dispatch(MoveElem(pathFrom, pathTo, RelationType("has"))) >> dispatch(RemoveElem(pathFrom)) >> dispatch(RemoveEmptyRelation(pathFrom.init))
      }


    }


    def onDoubleClickTreeItem(P: NodeProps, S: NodeState)(e: ReactEvent): Callback = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB
      val path = (if (P.parent.isEmpty) P.root.uuid.toString else P.parent + "/" + P.root.uuid).split("/")

      P.root.item match {
        case _ : Elem => P.openEditModal(P.root, dispatch, path)
        case _ => Callback()
      }
    }


    def dragOver(P:NodeProps)(e: ReactDragEvent): Callback = {
      e.preventDefaultCB >> Callback(e.dataTransfer.dropEffect = "move")
    }

    def removeElem(P: NodeProps): Callback = {
      val path = if (P.parent.isEmpty) P.root.uuid.toString else P.parent + "/" + P.root.uuid
      val dispatch: Action => Callback = P.modelProxy.dispatchCB

      P.openDeleteModal(P.root, dispatch, path.split("/"))
    }

    def dragOverStyle(P: NodeProps): Seq[TagMod] = {
      Seq(^.opacity := 0.5,
        ^.border := "1px solid",
        ^.borderColor := "black",
        P.root.item.isInstanceOf[Attribute] ?= Seq(
          ^.color := "#FF3636",
          ^.borderColor := "#FF3636"
        )
      )
    }

    def setContentDivSize(content: String): Seq[TagMod] = {
      var cont : String = content
      val nbrInserts = content.length / 37

      for(i <- 1 to nbrInserts) {
        if(i == 1){
          cont = cont.substring(0, i*37) + "\n" + cont.substring(i*37, cont.length)
        } else {
          cont = cont.substring(0,i*36) + "...\n\n" + cont.substring(i*36, cont.length)
        }
      }
      Seq(
        cont,
        ^.whiteSpace := "pre-line"
      )
    }

    def render(P: NodeProps, S: NodeState): ReactTag = {
      val dispatch: Action => Callback = P.modelProxy.dispatchCB

      val depth = P.depth + 1

      val parent = if (P.parent.isEmpty) P.root.uuid.toString
      else s"${P.parent}/${P.root.uuid.toString}"

      val path = (if (P.parent.isEmpty) P.root.uuid.toString
      else P.parent + "/" + P.root.uuid).split("/")

      val updateRel = UpdateRelation(path, P.root.item.asInstanceOf[Entity].id, _: Option[RelationType])

      val collapsed = P.collapseProxy.value.find(_.uuid == P.root.uuid) match {
        case Some(tuple) =>
          tuple.collapsed
        case None =>
          P.collapseProxy.dispatchCB(AddTuple(Tuple(P.root.uuid, collapsed = false)))
          false
      }

      val treeMenuToggle: TagMod =
        if (S.children.nonEmpty && !collapsed)
        <.span(
          ^.onClick ==> onTreeMenuToggle(P),
          ^.onDblClick ==> onTreeMenuToggle(P),
          ^.key := "arrow",
          P.style.treeItemBefore,
          "▼"
        )
        else if (P.root.children.nonEmpty && S.children.isEmpty)
        <.span(
          ^.onClick ==> onTreeMenuToggle(P),
          ^.onDblClick ==> onTreeMenuToggle(P),
          ^.key := "arrow",
          P.style.treeItemBefore,
          "▶"
        )
        else ""


      <.li(
        P.style.treeItem,
        <.div(
          P.style.treeItemDiv,
          ^.borderBottomRightRadius := { if(P.root.children.isEmpty) "5px" else "0px" },
          ^.borderTopRightRadius := { if(P.root.children.isEmpty) "5px" else "0px" },
          ^.backgroundColor := { if (P.root.item.isInstanceOf[Entity]) "#CEDBE7" else "#CFEADD" },
          treeMenuToggle,
          ^.onDrop ==> onDrop(P),
          ^.onDragStart ==> dragStart(P),
          ^.onDragEnd ==> onItemDrop(P),
          ^.onDragOver ==> onItemDragOver(P),
          ^.onDblClick ==> onDoubleClickTreeItem(P,S),
          S.draggedOver ?= dragOverStyle(P),
          ^.onClick ==> onItemSelect(P),
          <.div(
            P.style.treeItemIdDiv,
            ^.id := P.root.itemToString,
            if (P.root.item.isInstanceOf[Elem]) {
              Seq(
                <.div(
                  P.style.elemDiv1,
                  ^.fontSize := { if(P.root.entityToString.length > 12) "small" else "medium" },
                  P.root.entityToString
                ),
                <.div(
                  P.style.elemDiv2
                ),
                <.div(
                  P.style.elemDiv3,
                  setContentDivSize(P.root.contentToString)
                )
              )
            } else{
              <.div(
                P.style.attributeDiv,
                <.span(
                  P.root.entityToString
                )
              )
            }
          ),
          <.button(
            Styles.bootStrapRemoveButton,
            ^.onClick --> removeElem(P)
          ),
          getRelationType(P.root) match {
            case Some(relation) =>
              <.div(
                ^.position.absolute,
                ^.top := "0%",
                ^.height := "100%",
                ^.left := "100%",
                RelationSelect(relation, dispatch, Some(updateRel), isModelValue = true, None, Some(P.saveScrollPosition(_)))
              )
            case None =>
              <.div()
          }
        ),
        <.ul(P.style.treeGroup)(
          S.children.map(child =>
            (matchesFilterText(P.filterText, child) || matchesFilterText(P.filterText, P.root)) ?=
              cc(proxy => TreeNode.withKey(s"$parent/${child.uuid}")(P.copy(
                root = child,
                open = P.open,
                depth = depth,
                parent = parent,
                filterText = P.filterText,
                collapseProxy = proxy
              ))
              )
          ))
      )
    }
  }

  val cc = CollapseCircuit.connect(_.list)

  case class NodeState(children: Seq[TreeItem], selected: Boolean = false, draggedOver: Boolean = false, scrollPosition: Double = 0)

  case class NodeProps(root: TreeItem,
                       open: Boolean,
                       depth: Int = 0,
                       parent: String = "",
                       onNodeSelect: (NodeC) => Callback,
                       onNodeDrag: (NodeC) => Callback,
                       onNodeDrop: (NodeC) => Callback,
                       filterText: String,
                       style: Style,
                       filterMode: Boolean,
                       modelProxy: ModelProxy[Tree],
                       openDeleteModal: (TreeItem, (Action => Callback),  Seq[String]) => Callback,
                       openEditModal: (TreeItem, (Action => Callback),  Seq[String]) => Callback,
                       openAddElemModal: (TreeItem, (Action => Callback),  Seq[String], Option[Elem]) => Callback,
                       saveScrollPosition: Double => Callback,
                       collapseProxy: ModelProxy[Seq[Tuple]]
                      )



  lazy val TreeNode = ReactComponentB[NodeProps]("ReactTreeNode")
    .initialState_P(P => NodeState(P.root.children))
    .renderBackend[NodeBackend]
      .componentWillMount(x =>
        x.props.collapseProxy.dispatchCB(AddTuple(Tuple(x.props.root.uuid, collapsed = false))) >>
        {
          x.props.collapseProxy.value.find(_.uuid == x.props.root.uuid) match {
            case Some(tuple) =>
              if(!tuple.collapsed)
                x.modState(_.copy(children = x.props.root.children))
              else
                x.modState(_.copy(children = Nil))
            case None =>
              x.modState(_.copy(children = x.props.root.children))
          }
        }
      )
    .componentWillReceiveProps {
      case ComponentWillReceiveProps(_$, newProps) =>
        _$.modState(_.copy(children = if (newProps.open) newProps.root.children else Nil))
          .when(newProps.filterMode)
          .void

    }
      .componentWillUpdate(x => {
        x.$.props.saveScrollPosition(document.getElementById("treeView").scrollTop)
      })
    .shouldComponentUpdate(x => if(x.nextState.selected || x.currentState.draggedOver) true else false)
    .build

  val component = ReactComponentB[Props]("ReactTreeView")
    .initialState(State("", filterMode = false, js.undefined, js.undefined, scrollPosition = 0))
    .renderBackend[Backend]
      .componentWillUpdate($ => {
        Callback($.nextState.copy(collapseList =  $.nextState.collapseList ++ $.currentState.collapseList))
      })
    .build


  def apply(root: TreeItem,
            openByDefault: Boolean = true,
            onItemSelect: js.UndefOr[(String, String, Int) => Callback] = js.undefined,
            showSearchBox: Boolean = false,
            ref: js.UndefOr[String] = js.undefined,
            key: js.UndefOr[js.Any] = js.undefined,
            style: Style = new Style {},
            modelProxy: ModelProxy[Tree],
            saveScrollPosition: Double => Callback

           ) =
    component.set(key, ref)(Props(root, openByDefault, onItemSelect, showSearchBox, style, modelProxy, saveScrollPosition))

}