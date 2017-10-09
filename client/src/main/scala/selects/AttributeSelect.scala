package selects

import japgolly.scalajs.react.vdom.html_<^.{<, _}
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent, _}
import shared._

object AttributeSelect {

  val selectStyle = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height := "100%",
    ^.color := "#03EE7D",
    ^.background := "white",
    ^.textAlign.center,
    ^.textAlignLast.center
  ).toTagMod

  case class Props(value: String, isIntAttr: Boolean, setNewAttribute: Option[Attribute] => Callback)

  case class State(value: String)

  val intAttributeList = List("Benefit", "Capacity", "Cost", "Damage", "Frequency", "Min", "Max", "Order", "Prio", "Probability", "Profit", "Value")

  val stringAttributeList = List("Code", "Comment", "Deprecated", "Example", "Expectation", "FileName", "Gist", "Image", "Spec", "Text", "Title", "Why")


  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      <.select(
        selectStyle,
        ^.value := {
          if (S.value.isEmpty) P.value else S.value
        },
        ^.onChange ==> onChange(P, S)
      )(
        intAttributeList.map(x => <.option(x)).toTagMod.when(P.isIntAttr),
        stringAttributeList.map(x => <.option(x)).toTagMod.when(!P.isIntAttr)
      )


    def onChange(P: Props, S: State)(e: ReactEventFromInput): Callback = {
      val newAttr = e.target.value
      e.preventDefault()

      if (P.isIntAttr) {
        val attribute = Some(IntAttribute(newAttr))
        P.setNewAttribute(attribute) >> $.setState(S.copy(value = newAttr))
      } else {
        val attribute = Some(StringAttribute(newAttr))
        P.setNewAttribute(attribute) >> $.setState(S.copy(value = newAttr))
      }
    }
  }

  val component = ScalaComponent.builder[Props]("RelationSelect")
    .initialState(State(value = ""))
    .renderBackend[Backend]
    .build


  def apply(value: String, isIntAttr: Boolean, setNewAttribute: Option[Attribute] => Callback)
  = component(Props(value, isIntAttr, setNewAttribute))()

}
