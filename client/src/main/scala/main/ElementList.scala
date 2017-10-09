package main

/**
  * Created by phiped on 5/25/17.
  */

import scalacss.ProdDefaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import japgolly.scalajs.react._
import shared._
import upickle.default.write


object ElementList {

  object Style extends StyleSheet.Inline {

    import dsl._

    val dragListStyle = style(
      overflow.hidden,
      height(46.%%),
      width(100.%%)
    )

    val listElemStyle = style(
      boxShadow := "0px 6px 12px 0px rgba(0,0,0,0.2)",
      borderRadius(5.px),
      padding(5.px),
      cursor.grab
    )

    val entityListStyle = style(
      height(86.%%),
      marginTop(5.px),
      overflow.auto
    )
  }

  val entities = List("Ent", "Meta", "Item", "Label", "Section", "Term", "Actor", "App", "Component", "Domain", "Module", "Product", "Release", "Resource", "Risk", "Service",
    "Stakeholder", "System", "User", "Class", "Data", "Input", "Member", "Output", "Relationship", "Design", "Screen", "MockUp", "Function", "Interface", "State", "Event",
    "Epic", "Feature", "Goal", "Idea", "Issue", "Req", "Ticket", "WorkPackage", "Breakpoint", "Barrier", "Quality", "Target", "Scenario", "Task", "Test", "Story", "UseCase",
    "VariationPoint", "Variant")
  val intAttribute = List("Benefit", "Capacity", "Cost", "Damage", "Frequency", "Min", "Max", "Order", "Prio", "Probability", "Profit", "Value")
  val stringAttribute = List("Comment", "Deprecated", "Example", "Expectation", "FileName", "Gist", "Image", "Spec", "Text", "Title", "Why")

  val attributes: List[String] = intAttribute ++ stringAttribute
  val elems: List[String] = entities ++ attributes

  case class State(elems: Seq[String], showEntity: Boolean = false, showAttribute: Boolean = false)

  case class Props()

  def dragStart(elem: Elem)(event: ReactDragEvent): Callback = {
    event.dataTransfer.effectAllowed = "move"
    event.dataTransfer.setData("existing", "false")
    elem.setRandomUUID()

    elem match {
      case entity: Entity =>
        event.dataTransfer.setData("type", "entity")
        Callback(event.dataTransfer.setData("elem", write[Entity](entity)))
      case attribute: StringAttribute =>
        event.dataTransfer.setData("type", "stringAttr")
        Callback(event.dataTransfer.setData("elem", write[StringAttribute](attribute)))
      case attribute: IntAttribute =>
        event.dataTransfer.setData("type", "intAttr")
        Callback(event.dataTransfer.setData("elem", write[IntAttribute](attribute)))
      case _ =>
        event.dataTransfer.setData("type", "invalid")
        Callback(println("Dragged element is not valid"))
    }
  }


  val listElem = ScalaComponent.builder[String]("listElem")
    .render($ => <.ul(
      Style.listElemStyle,
      ^.draggable := true,
      ^.id := $.props.toString,
      ^.classID := $.props.toString,
      $.props.toString.takeWhile(_ != '('),
      ^.background := (if (entities.contains($.props)) "#CEDBE7" else "#CFEADD"),
      ^.onDragStart ==> (
        if (entities.contains($.props)) dragStart(Entity($.props))
        else if (intAttribute.contains($.props)) dragStart(IntAttribute($.props))
        else dragStart(StringAttribute($.props))
        )
    )
    )
    .build

  val entityListView = ScalaComponent.builder[Seq[String]]("entityList")
    .render(elems => <.pre(
      ^.className := "form-control",
      ^.id := "dragList",
      Style.entityListStyle,
      elems.props.sorted.map(listElem(_)).toTagMod
    ))
    .build


  class Backend($: BackendScope[Unit, State]) {
    def render(S: State) = {
      <.pre(
        Style.dragListStyle,
        searchBox(),
        checkBoxes(S),
        entityListView(S.elems)
      )
    }

    val searchBox = ScalaComponent.builder[Unit]("searchBox")
      .render(_ => <.form(
        <.input.text(
          ^.className := "form-control",
          ^.placeholder := "Search",
          ^.onChange ==> onTextChange
        )
      )
      ).build

    val checkBoxes = ScalaComponent.builder[State]("checkBoxes")
      .render($ =>
        <.div(
          <.input.checkbox(
            ^.onChange ==> toggleEntity($.props)
          ),
          " Entity ",
          <.input.checkbox(
            ^.onChange ==> toggleAttribute($.props)
          ),
          " Attribute "
        ))
      .build

    def toggleEntity(S: State)(event: ReactEventFromInput): Callback = {
      if (S.showEntity && S.showAttribute)
        $.modState(_.copy(elems = attributes, showEntity = !S.showEntity))
      else if (S.showAttribute || S.showEntity)
        $.modState(_.copy(elems = elems, showEntity = !S.showEntity))
      else
        $.modState(_.copy(elems = entities, showEntity = !S.showEntity))
    }

    def toggleAttribute(S: State)(event: ReactEventFromInput): Callback = {
      if (S.showAttribute && S.showEntity)
        $.modState(_.copy(elems = entities, showAttribute = !S.showAttribute))
      else if (S.showAttribute || S.showEntity)
        $.modState(_.copy(elems = elems, showAttribute = !S.showAttribute))
      else
        $.modState(_.copy(elems = attributes, showAttribute = !S.showAttribute))
    }

    def onTextChange(event: ReactEventFromInput): Callback =
      event.extract(_.target.value.toLowerCase) {
        case "entity" | "entities" => $.modState(_.copy(elems = entities))
        case "attribute" | "attributes" => $.modState(_.copy(elems = attributes))
        case value => $.modState(_.copy(elems = elems.filter(_.toLowerCase.contains(value.toLowerCase))))
      }
  }


  val component = ScalaComponent.builder[Unit]("ElementList")
    .initialState(State(elems = elems))
    .renderBackend[Backend]
    .build


  def apply() = component()

}

