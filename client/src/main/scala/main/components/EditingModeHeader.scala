package main.components

import scalacss.ScalaCssReact._
import scalacss.Defaults._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy

object EditingModeHeader {

  object Style extends StyleSheet.Inline {
    import dsl._

    val viewModeLabel = style(
      display.inlineBlock,
      marginRight(4.px)
    )

    val delimiter = style(
      display.inlineBlock,
      width(6.px)
    )

    val editingModeButton = style(
      minWidth(70.px),
      minHeight(35.px)
    )

    val editingMode = style(
      marginLeft(10.px),
      display.inlineBlock,
      padding(4.px),
      backgroundColor(c"#f5f5f5"),
      borderTop(1.px, solid, c"rgb(204, 204, 204)"),
      borderRight(1.px, solid, c"rgb(204, 204, 204)"),
      borderLeft(1.px, solid, c"rgb(204, 204, 204)"),
      borderTopLeftRadius(4.px),
      borderTopRightRadius(4.px)
    )
  }

  object Buttons extends Enumeration {
    val Text, Tree = Value
  }

  case class State(activeButton: Buttons.Value)

  def editingModeButton(name: String, isActive: Boolean, setButton: Callback) = Seq(
      <.button(
        ^.className := "btn" ++ (if (isActive) " btn-primary" else " btn-default"),
        ^.onClick --> setButton,
        Style.editingModeButton,
        name)
    )

  val editingModeHeader = ReactComponentB[State]("editingModeHeader")
      .initialState(State(activeButton = Buttons.Tree))
    .render($ => <.div(
      Style.editingMode,
      <.p(Style.viewModeLabel, "View"),
      editingModeButton(
        "Tree",
        isActive = $.state.activeButton == Buttons.Tree,
        $.modState(_.copy(activeButton = Buttons.Tree))
      ),
      <.div(Style.delimiter), // Delimiter "invisible block"
      editingModeButton(
        "Text",
        isActive = $.state.activeButton == Buttons.Text,
        $.modState(_.copy(activeButton = Buttons.Text))
      )
    )
    ).build

  def apply() = editingModeHeader.set()(State(activeButton = Buttons.Tree))
}
