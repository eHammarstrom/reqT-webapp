package modals

import diode.Action
import main.TreeItem
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent, _}
import org.scalajs.dom.ext.KeyCode
import shared._


object DeleteModal {

  val modalStyle: TagMod = Seq(
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

  val buttonAreaStyle: TagMod = Seq(
    ^.width := "95%",
    ^.padding := "20px",
    ^.display.flex,
    ^.justifyContent.spaceBetween
  ).toTagMod

  case class State()

  case class Props(isOpen: Boolean, onClose: Callback, treeItem: TreeItem = null, dispatch: (Action => Callback) = null, path: Seq[String] = Seq())

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.autoFocus := true,
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            deleteElemModalStyle(P, S)
          ),
          <.div(
            backdropStyle,
            ^.onClick --> P.onClose
          )
        )
      } else
        <.div()

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventFromInput): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        P.onClose
      } else if (e.nativeEvent.keyCode == KeyCode.Enter) {
        onDelete(P)
      } else {
        Callback()
      }
    }

    def deleteElemModalStyle(P: Props, S: State): TagMod =
      P.treeItem.item match {
        case "Model" => Seq(
          <.h5(
            "Do you want to delete the entire model?",
            ^.fontSize := "16px",
            ^.textAlign.center
          ),
          <.div(
            buttonAreaStyle,
            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick --> P.onClose),
            <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick --> onDelete(P))
          )
        ).toTagMod
        case item => Seq(
          <.h5(
            "Do you want to delete the following?",
            ^.fontSize := "16px",
            ^.textAlign.center
          ),
          <.dl(
            <.br,
            ^.className := "dl-horizontal",
            <.dt(
              ^.textAlign := "center",
              ^.color := {
                if (item.isInstanceOf[IntAttribute] || item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
              },
              P.treeItem.nodeToString),
            <.dd(
              ^.whiteSpace := "pre-line",
              P.treeItem.contentToString
            ),
            <.hr.when(P.treeItem.children.nonEmpty),
            <.dt(
              ^.textAlign := "center",
              ^.color := "#FF3636",
              P.treeItem.linkToString
            ),
            <.dl(
              ^.className := "dl-horizontal"
            ),
            <.br.when(P.treeItem.children.nonEmpty),
            <.hr,
            <.div(
              ^.maxHeight := "300px",
              ^.overflowY := "auto",
              P.treeItem.children.map(child => {
                (Seq(
                  <.dt(
                    child.nodeToString.replaceAll("TreeItem", ""),
                    ^.textAlign := "center",
                    ^.color := {
                      if (child.item.isInstanceOf[IntAttribute] || child.item.isInstanceOf[StringAttribute]) "#03EE7D" else "#047BEA"
                    }
                  ),
                  <.dd(
                    ^.whiteSpace := "pre-line",
                    ^.wordBreak := "break-word",
                    child.contentToString
                  ),
                  <.br
                ): Seq[TagMod]).toTagMod
              }).toTagMod
            ),
            <.br.when(P.treeItem.children.nonEmpty)
          ),
          <.div(
            buttonAreaStyle,
            <.button("Cancel", ^.className := "btn btn-default", ^.bottom := "0px", ^.onClick --> P.onClose),
            <.button("Delete", ^.className := "btn btn-danger", ^.bottom := "0px", ^.onClick --> onDelete(P))
          )
        ).toTagMod
      }


    def onDelete(P: Props): Callback = {
      P.treeItem.item match {
        case _: String =>
          P.dispatch(RemoveElem(P.path)) >> P.onClose
        case _ =>
          P.dispatch(RemoveElem(P.path)) >> P.dispatch(RemoveEmptyRelation(P.path.init)) >> P.onClose
      }
    }

  }


  val component = ScalaComponent.builder[Props]("Modal")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(isOpen: Boolean, onClose: Callback, treeItem: TreeItem, dispatch: (Action => Callback), path: Seq[String])
  = component(Props(isOpen, onClose, treeItem, dispatch, path))
}
