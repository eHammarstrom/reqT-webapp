package modals

import diode.Action
import example.{TreeItem}
import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, _}
import org.scalajs.dom.ext.KeyCode
import shared._


object AddElemModal {

  def modalStyle = Seq(
    ^.width := "400px",
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
  )

  def backdropStyle = Seq(
    ^.position := "absolute",
    ^.width := "100%",
    ^.height := "100%",
    ^.top := "0px",
    ^.left := "0px",
    ^.zIndex := "9998",
    ^.background := "#CCC",
    ^.opacity := "0.5"
  )

  def buttonAreaStyle = Seq(
    ^.width:= "95%",
    ^.padding := "20px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  )

  def intInputStyle = Seq(
    ^.className := "form-control",
    ^.width := "60%",
    ^.marginTop := "-18px",
    ^.borderRadius := "5px",
    ^.autoFocus := "true",
    ^.maxLength := "9",
    ^.placeholder := "Number"
  )

  def textAreaStyle = Seq(
    ^.className := "form-control",
    ^.width := "95%",
    ^.maxWidth := "95%",
    ^.maxHeight := "200px",
    ^.marginTop := "-18px",
    ^.border := "1px solid #CCC",
    ^.borderRadius := "5px",
    ^.autoFocus := "true"
  )


  case class State(input: String)

  case class Props(isOpen: Boolean, onClose: Callback, treeItem: TreeItem = null, prepDispatch: (Seq[String], Elem) => Callback = null, path: Seq[String] = Seq(), elemToAdd: Option[Elem])

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S:State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P,S),
          <.div(
            modalStyle,
            addElemModalStyle(P,S)
          ),
          <.div(
            backdropStyle,
            ^.onClick --> P.onClose
          )
        )
      }else
        <.div()

    def inputChanged(e: ReactEventI): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput))
    }

    def intInputChanged(e: ReactEventI): Callback = {
      val newInput = e.target.value
      $.modState(_.copy(input = newInput.replaceAll("[^\\d]", "")))
    }

    def addElem(P: Props, S: State): Callback = {

        P.prepDispatch(P.path, prepareElem(S.input, P.elemToAdd)) >> P.onClose

//        P.dispatch(AddElem(P.path, prepareElem(S.input, P.elemToAdd), RelationType("has"))) >> P.onClose
    }

    def prepareElem(newValue: String, elem: Option[Elem]): Elem = {
      elem.get match {
        case entity: Entity => entity.setID(newValue)
        case intAttr: IntAttribute => intAttr.setValue(newValue.replace(" ", "").toInt)
        case stringAttr: StringAttribute => stringAttr.setValue(newValue)
        case _ => elem.get
      }
    }

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape){
        P.onClose
      } else if(e.nativeEvent.keyCode == KeyCode.Enter &&  !e.shiftKey){
        addElem(P,S)
      }else{
        Callback()
      }
    }

    def addElemModalStyle(P: Props, S: State) = Seq(
      <.h4(
        "Do you want to add the following?",
        ^.textAlign.center
      ),
      <.dl(
        ^.className := "dl-horizontal",
        <.br,
        <.dt(
          P.treeItem.entityToString,
          ^.textAlign := "center",
          ^.color := {
            if (P.treeItem.isInstanceOf[Attribute]) "#03EE7D" else "#047BEA"
          }
        ),
        <.dd(
          ^.marginTop := "-18px",
          {
            if (P.treeItem.entityToString != "Model") P.treeItem.contentToString else ""
          }

        ),
        <.hr,
        <.dt(
          ^.textAlign := "center",
          ^.color := "#FF3636",
          "has"
        ),
        <.dd(

        ),
        <.hr,
        <.dt(
          P.elemToAdd match {
            case Some(e: Entity) => e.getType
            case Some(e: IntAttribute) => e.getType
            case Some(e: StringAttribute) => e.getType
            case Some(e: Relation) => "Error"
            case None => "Error"
          },
          ^.textAlign := "center",
          ^.color := {
            if (P.elemToAdd.get.isEntity) "#047BEA" else "#03EE7D"
          }
        ),
        <.dd(
          if (P.elemToAdd.get.isIntAttribute) {
            <.input(
              intInputStyle,
              ^.value := S.input,
              ^.onChange ==> intInputChanged
            )
          } else {
            <.textarea(
              textAreaStyle,
              ^.placeholder := {
                if (P.elemToAdd.get.isEntity) "Id" else "Description"
              },
              ^.onChange ==> inputChanged
            )
          }
        ),
        <.br
      ),
      <.div(
        buttonAreaStyle,
        <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> P.onClose),
        <.button("Add", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.disabled := S.input.isEmpty, ^.onClick --> addElem(P, S))
      )
    )


  }


  val component = ReactComponentB[Props]("Modal")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  def apply(isOpen: Boolean, onClose: Callback, treeItem: TreeItem, prepDispatch: (Seq[String], Elem) => Callback, path: Seq[String], elemToAdd: Option[Elem])
  = component.set()(Props(isOpen, onClose, treeItem, prepDispatch, path, elemToAdd))
}
