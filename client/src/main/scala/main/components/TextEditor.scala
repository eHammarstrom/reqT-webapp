package main.components

import scalacss.ScalaCssReact._
import scalacss.Defaults._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import japgolly.scalajs.react.vdom.prefix_<^.{<, _}
import japgolly.scalajs.react._
import diode.react.ModelProxy

import shared._

object TextEditor {

  object Style extends StyleSheet.Inline {

  }

  val component = ReactComponentB[ModelProxy[Tree]]("TextEditor")
    .render(P => <.pre(
      <.p("Under construction, want to help? OSS @ github.com/reqT/reqT-webapp")
    ))
    .build

  def apply(modelProxy: ModelProxy[Tree]) = component(modelProxy)
}
