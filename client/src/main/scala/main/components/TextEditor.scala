package main.components

import scalacss.ScalaCssReact._
import scalacss.Defaults._
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy
import org.scalajs.dom
import org.scalajs.dom.Element
import shared._

object TextEditor {

  object Style extends StyleSheet.Inline {

    import dsl._

    val codeContainer = style(
      width(100.%%),
      minHeight(500.px),
      overflow.scroll
    )
  }

  // github@chandu0101/scalajs-react-components
  def applySyntaxHighlight = Callback {
    import scala.scalajs.js.Dynamic.{global => g}
    val e: Element = dom.document.querySelector("#text-editor")
    g.hljs.highlightBlock(e)
    println("Applied code style!")
  }

  val source =
    """
      |package scalajs
      |
      |import something.funny._
      |
      |object Main {
      | println("Hello world!")
      |}
      |
    """.stripMargin

  val textEditor = <.code(
        Style.codeContainer,
        ^.id := "text-editor",
        ^.className := "scala",
        ^.contentEditable := true,
        ^.onKeyDown --> applySyntaxHighlight,
        source
      )

  val component = ReactComponentB[ModelProxy[Tree]]("TextEditorComponent")
    .render(_ => <.pre(
      <.p(
        "Under construction, want to help? ",
        <.a(^.href := "https:github.com/reqT/reqT-webapp", "github repo")
      ),
      textEditor
    ))
    .componentDidMount(_ => applySyntaxHighlight)
    .componentDidUpdate(_ => applySyntaxHighlight)
    .build

  def apply(modelProxy: ModelProxy[Tree]) = component(modelProxy)
}
