package example


import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEvent, ReactKeyboardEventI}
import org.scalajs.dom.ext.KeyCode
import japgolly.scalajs.react._
import shared.{Attribute, Entity}

/**
  * Created by johan on 5/17/17.
  */
object NewModelModal {
  def modalStyle = Seq(
    ^.width:= "400px",
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

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, addToCachedModels: (String, Boolean) => Callback)

  case class State(newModelName: String)

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            <.h4(
              "Specify name of the new model"
            ),
            <.input(
              ^.`type` := "text",
              ^.className := "form-control",
              ^.placeholder := "Enter name",
              ^.value := S.newModelName,
              ^.onChange ==> onChange(P,S)
            ),
            <.div(
              ^.width := "95%",
              ^.padding := "20px",
              ^.display.flex,
              ^.justifyContent.spaceBetween,
              <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
              <.button("Add empty model", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> addModel(false, P, S)),
              <.button("Add current model", ^.className := "btn btn-success pull-right", ^.autoFocus := "true", ^.bottom := "0px", ^.onClick ==> addModel(true,P, S))
            )
          ),
          <.div(
            backdropStyle,
            ^.onClick ==> onClose(P)
          )
        )

      } else
        <.div()

    def onChange(P: Props, S: State)(e: ReactEventI): Callback = {
      e.preventDefault()
      val newName= e.target.value
      $.setState(s = S.copy(newModelName = newName))
    }

    def addModel(isCurrModel: Boolean, P: Props, S: State)(e: ReactEvent): Callback = {
      println(S.newModelName)
      P.addToCachedModels(S.newModelName, isCurrModel) >> onClose(P)(e)
    }

    def resetState: Callback = $.setState(State(""))

    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) >> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)(e)
      }
      else
        Callback()
    }
  }

  val component = ReactComponentB[Props]("Modal")
    .initialState(State(""))
    .renderBackend[Backend]
    .build


  def apply(isOpen: Boolean, onClose: ReactEvent => Callback, addToCachedModels: (String, Boolean) => Callback)
  = component.set()(Props(isOpen, onClose, addToCachedModels))
}
