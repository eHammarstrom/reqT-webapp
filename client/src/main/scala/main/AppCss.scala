package main


import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry


object AppCss {

  def load = {
    GlobalRegistry.register(
      GlobalStyle,
      ElementList.Style,
      components.EditingModeHeader.Style
    )

    GlobalRegistry.onRegistration(_.addToDocument())
  }

}
