package modals

import diode.Action
import main.TreeItem
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent, _}
import org.scalajs.dom.ext.KeyCode
import selects.{AttributeSelect, EntitySelect, RelationSelect}
import shared._


object EditModal {

  val modalStyle: TagMod = Seq(
    ^.width := "80%",
    ^.minWidth := "400px",
    ^.padding := "5px",
    ^.position := "absolute",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.top := "40%",
    ^.left := "50%",
    ^.transform := "translate(-50%,-50%)",
    ^.zIndex := "9999",
    ^.background := "#FFF",
    ^.paddingBottom := "15px",
    ^.paddingRight := "15px",
    ^.paddingTop := "15px",
    ^.paddingLeft := "15px",
    ^.boxShadow := "rgba(0, 0, 0, 0.2) 5px 6px 12px 0px"
  ).toTagMod

  val backdropStyle: TagMod = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998",
    ^.background := "#CCC",
    ^.opacity := "0.5"
  ).toTagMod

  val intInputStyle: TagMod = Seq(
    ^.className := "form-control",
    ^.width := "60%",
    ^.whiteSpace := "pre-line",
    ^.borderRadius := "5px",
    ^.marginTop := "-18px",
    ^.autoFocus := true,
    ^.maxLength := "9",
    ^.placeholder := "Number"
  ).toTagMod

  val textAreaStyle: TagMod = Seq(
    ^.className := "form-control",
    ^.width := "95%",
    ^.maxWidth := "95%",
    ^.marginTop := "-18px",
    ^.maxHeight := "200px",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.autoFocus := true
  ).toTagMod

  val buttonAreaStyle: TagMod = Seq(
    ^.width := "95%",
    ^.padding := "20px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  ).toTagMod

  case class State(input: String, newEntity: Option[Entity] = None, newRelation: Option[RelationType] = None, newAttribute: Option[Attribute] = None)

  case class Props(isOpen: Boolean, onClose: Callback, treeItem: TreeItem = null, dispatch: (Action => Callback) = null, path: Seq[String] = Seq())

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            editModalStyle(P, S)
          ),
          <.div(
            backdropStyle,
            ^.onClick --> P.onClose
          )
        )
      } else
        <.div()


    def inputChanged(e: ReactEventFromInput): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput))
    }

    def intInputChanged(e: ReactEventFromInput): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput.replaceAll("[^\\d]", "")))
    }

    def setNewEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newEntity = entity))

    def setNewRelation(relationType: Option[RelationType]): Callback = $.modState(_.copy(newRelation = relationType))

    def setNewAttribute(attribute: Option[Attribute]): Callback = $.modState(_.copy(newAttribute = attribute))

    def onSave(P: Props, S: State): Callback = {
      P.treeItem.item match {
        case entity: Entity => if (entity.hasRelation) {
          S.newEntity.get.setID(S.input)
          S.newEntity.get.hasRelation = true
          P.dispatch(UpdateEntireRelation(path = P.path, newEntity = S.newEntity.get, S.newRelation)) >> P.onClose
        } else {
          S.newEntity.get.setID(S.input)
          P.dispatch(UpdateEntity(path = P.path, newEntity = S.newEntity.get)) >> P.onClose
        }

        case _: IntAttribute =>
          P.dispatch(UpdateIntAttribute(path = P.path, S.newAttribute.getOrElse(P.treeItem.item).asInstanceOf[IntAttribute].setValue(S.input.toInt))) >> P.onClose
        case _: StringAttribute =>
          P.dispatch(UpdateStringAttribute(path = P.path, S.newAttribute.getOrElse(P.treeItem.item).asInstanceOf[StringAttribute].setValue(S.input))) >> P.onClose

      }
    }

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventFromInput): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        P.onClose
      } else if (e.nativeEvent.keyCode == KeyCode.Enter && !e.shiftKey) {
        onSave(P, S)
      } else {
        Callback()
      }
    }

    def editModalStyle(P: Props, S: State): TagMod = Seq(
      <.h4(
        "Edit element",
        ^.textAlign.center
      ),
      <.dl(
        <.br,
        ^.className := "dl-horizontal",
        <.dt(
          P.treeItem.item match {
            case intAttr: IntAttribute => <.div(
              ^.textAlign := "center",
              ^.color := "#03EE7D",
              AttributeSelect(intAttr.getType, isIntAttr = true, setNewAttribute)
            )
            case stringAttr: StringAttribute => <.div(
              ^.textAlign := "center",
              ^.color := "#03EE7D",
              AttributeSelect(stringAttr.getType, isIntAttr = false, setNewAttribute)
            )
            case _: Entity => EntitySelect(P.treeItem.nodeToString, setNewEntity, isModelValue = false)
          }),
        <.dd(
          if (P.treeItem.item.isInstanceOf[IntAttribute]) {
            <.input(
              intInputStyle,
              ^.value := S.input,
              ^.onChange ==> intInputChanged
            )
          } else {
            <.textarea(
              textAreaStyle,
              ^.rows := {
                if (S.input.length < 28) 2 else 4
              },
              ^.value := S.input,
              ^.onChange ==> inputChanged
            )
          }
        ),
        <.hr.when(P.treeItem.children.nonEmpty),
        <.dt(
          ^.textAlign := "center",
          ^.color := "#FF3636",
          ^.position.relative,
          if (P.treeItem.children.nonEmpty) {
            RelationSelect(P.treeItem.linkToString, Some(P.dispatch), None, isModelValue = false, Some(setNewRelation), None)
          } else {
            <.div()
          }
        ),
        <.dd(
        ),
        <.hr.when(P.treeItem.children.nonEmpty),
        <.div(
          ^.maxHeight := "300px",
          ^.overflowY := "auto",
          P.treeItem.children.map(x => {
            (Seq(
              <.dt(
                x.nodeToString.replaceAll("TreeItem", ""),
                ^.textAlign := "center",
                ^.paddingRight := "3.5%",
                ^.color := {
                  if (x.item.isInstanceOf[StringAttribute] || x.item.isInstanceOf[IntAttribute]) "#03EE7D" else "#047BEA"
                }
              ),
              <.dd(
                ^.whiteSpace := "pre-line",
                ^.wordBreak := "break-word",
                x.contentToString
              ),
              <.br
            ): Seq[TagMod]).toTagMod
          }).toTagMod
        )
      ),
      <.div(
        buttonAreaStyle,
        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> P.onClose),
        <.button("Save Changes", ^.className := "btn btn-success pull-right", ^.disabled := S.input.isEmpty, ^.bottom := "0px", ^.onClick --> onSave(P, S))
      )
    ).toTagMod


    def initStates(newProps: Props): Callback = {
      if (newProps.treeItem != null) {

        val newInput = if (newProps.treeItem.item != "Model") newProps.treeItem.contentToString else ""


        newProps.treeItem.item match {
          case entity: Entity =>
            $.modState(_.copy(input = newInput, newEntity = Some(entity), newRelation = newProps.treeItem.link, newAttribute = None))
          case attribute: IntAttribute =>
            $.modState(_.copy(input = newInput, newEntity = None, newRelation = newProps.treeItem.link, newAttribute = Some(attribute)))
          case attribute: StringAttribute =>
            $.modState(_.copy(input = newInput, newEntity = None, newRelation = newProps.treeItem.link, newAttribute = Some(attribute)))
          case _ =>
            $.modState(_.copy(input = newInput, newEntity = None, newRelation = None, newAttribute = None))
        }
      } else {
        Callback()
      }
    }


  }


  val component = ScalaComponent.builder[Props]("Modal")
    .initialState(State(""))
    .renderBackend[Backend]
    .componentWillReceiveProps(i => i.backend.initStates(i.nextProps))
    .build

  def apply(isOpen: Boolean, onClose: Callback, treeItem: TreeItem, dispatch: (Action => Callback), path: Seq[String])
  = component(Props(isOpen, onClose, treeItem, dispatch, path))
}
