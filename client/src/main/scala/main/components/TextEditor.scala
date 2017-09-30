package main.components

import scalacss.ScalaCssReact._
import scalacss.Defaults._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy
import org.scalajs.dom
import org.scalajs.dom.ext.PimpedNodeList
import shared._

object TextEditor {

  object Style extends StyleSheet.Inline {
    import dsl._

    val codeContainer = style(
      userSelect := "none"
    )
  }

  val component = ReactComponentB[ModelProxy[Tree]]("TextEditor")
    .render(P => <.pre(
      <.p("Under construction, want to help? OSS @ github.com/reqT/reqT-webapp"),
      <.code(
        Style.codeContainer,
        ^.className := "scala",
        //^.contentEditable := true,
        """
          |package scalajs
          |
          |import something.funny._
          |
          |object Main {
          | println("Hello world!")
          |}
        """.stripMargin)
    ))
    .componentDidMount(_ => applySyntaxHighlight)
    .componentDidUpdate(_ => applySyntaxHighlight)
    .build

  // github@chandu0101/scalajs-react-components
  def applySyntaxHighlight = Callback {
    import scala.scalajs.js.Dynamic.{global => g}
    val nodeList = dom.document.querySelectorAll("code")
    //nodeList.foreach(n => println(n.textContent))
    nodeList.foreach(n => g.hljs.highlightBlock(n))
  }

  def apply(modelProxy: ModelProxy[Tree]) = component(modelProxy)
}
