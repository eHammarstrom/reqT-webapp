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

  def editingModeButton(name: String, isActive: Boolean,
                        setButton: Callback, followUpCb: Callback = Callback()) =
    Seq(
      <.button(
        ^.className := "btn" ++ (if (isActive) " btn-primary" else " btn-default"),
        ^.onClick --> (setButton >> followUpCb),
        Style.editingModeButton,
        name)
    )

  case class State(activeButton: Buttons.Value)

  case class Props(setTreeView: Callback, setTextView: Callback)

  val editingModeHeader = ReactComponentB[(Props, State)]("editingModeHeader")
    .initialState(State(activeButton = Buttons.Tree))
    .render($ =>
      <.div(
      Style.editingMode,
      //<.p(Style.viewModeLabel, "View"),
      editingModeButton(
        "Tree",
        isActive = $.state.activeButton == Buttons.Tree,
        $.modState(_.copy(activeButton = Buttons.Tree)),
        $.props._1.setTreeView
      ),
      <.div(Style.delimiter), // Delimiter "invisible block"
      editingModeButton(
        "Text",
        isActive = $.state.activeButton == Buttons.Text,
        $.modState(_.copy(activeButton = Buttons.Text)),
        $.props._1.setTextView
      )
    )
    ).build

  def apply(setTreeView: Callback, setTextView: Callback) =
    editingModeHeader.set()((Props(setTreeView, setTextView), State(Buttons.Tree)))
}
