package modals

import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEventI, ReactKeyboardEventI, _}
import org.scalajs.dom.ext.KeyCode
import selects.AttributeSelect
import shared._


object ReleaseModal {

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
    ^.background := "#FFF" ,
    ^.paddingBottom := "15px",
    ^.paddingRight := "15px" ,
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

  def selectStyle() = Seq(
    ^.className := "form-control pull-right",
    ^.width := "155px",
    ^.height :=  "100%",
    ^.color := "BLACK",
    ^.background := "white",
    ^.textAlign.center,
    ^.textAlignLast.center
  )

  val standardList = Seq("Release", "Feature", "Stakeholder", "Resource")

  sealed trait ReleaseType
  case object MAX extends ReleaseType
  case object MIN extends ReleaseType

  case class State(newEntity: String, newAttribute: Option[Attribute], sortBy: Seq[String], releaseType: ReleaseType = MAX)

  case class Props(isOpen: Boolean, onClose: () => Callback, sendMethod: Seq[String] => Callback, currentModel: Tree)

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            <.h4(
              "Release Planning",
              ^.textAlign.center
            ),
            <.dl(
              ^.className := "dl-horizontal",
              <.dt(
                ^.textAlign.center,
                "Max/Min"
              ),
              <.dd(
                <.select(
                  selectStyle(),
                  ^.onChange ==> changeType(S)
                )(
                  <.option("Max"),
                  <.option("Min")
                )
              ),
              <.dt(
                ^.textAlign.center,
                "on "
              ),
              <.dd(
                AttributeSelect("Benefit", isIntAttr = true, setNewAttribute)
              ),
              <.dt(
                ^.textAlign.center,
                " of Entity "
              ),
              <.dd(
                entitySelect(P)
              )
            ),
            <.div(
              ^.width:= "95%",
              ^.padding := "20px",
              ^.display.flex,
              ^.justifyContent.spaceBetween,
              <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick --> onClose(P)),
              <.button("OK", ^.className := "btn btn-success pull-right", ^.autoFocus := "true", ^.bottom := "0px", ^.onClick --> sendMethod(P,S))
            )),
          <.div(
            backdropStyle,
            ^.onClick --> onClose(P)
          )
        )
      } else
        <.div()


//    val orderSelect = ReactComponentB[Int]("orderSelect")
//      .render($ =>
//        <.select(
//          selectStyle(),
//          ^.defaultValue := standardList($.props),
//          ^.onChange ==> changeOrder($.props)
//        )(
//          <.option("Release"),
//          <.option("Feature"),
//          <.option("Stakeholder"),
//          <.option("Resource")
//        )
//      )
//      .build


    val entitySelect = ReactComponentB[Props]("entitySelect")
      .render($ =>
        <.select(
          selectStyle(),
          ^.onChange ==> changeEntity
        )(
          createOptions($.props)
        )
      )
      .build

    def createOptions(P: Props): Seq[TagMod] = {
      val entities = getAllEntities(P.currentModel.children)
      entities.map(e => <.option(e.toString.replaceAll("\"","")))
    }


    def getAllEntities(elems: Seq[Elem]): Seq[Entity] = {
      if (elems.isEmpty){
        Seq()
      }else{
        val noAttr = elems.filter(_.isEntity).asInstanceOf[Seq[Entity]]
        val relations = elems.filter(_.isRelation)
        val children = relations.flatMap(_.asInstanceOf[Relation].submodel.children)

        noAttr ++ relations.map(_.asInstanceOf[Relation].entity) ++ getAllEntities(children)
      }
    }

    def changeEntity(e: ReactEventI): Callback = {
      val choosenEntity = e.target.value
      $.modState(_.copy(newEntity = choosenEntity))
    }


    def changeType(S: State)(e: ReactEventI): Callback =
      $.modState(_.copy(releaseType = S.releaseType match {
        case MAX => MIN
        case MIN => MAX
      }))


    def changeOrder(index: Int)(e: ReactEventI): Callback = {
      val newAttr = e.target.value
      $.modState(S => S.copy(sortBy = S.sortBy.updated(index,newAttr)))
    }



    def prepRelease(state: State, model: String): Seq[String] = {
      val entity = state.newEntity.replaceFirst("\\(", "(\"").reverse.replaceFirst("\\)", ")\"").reverse
      val attribute = state.newAttribute.getOrElse("Benefit").toString.split('(').head
      val problemType = state.releaseType match {
        case MAX => "maximize"
        case MIN => "minimize"
      }

      Seq(
        s"val releaseMethod=$model\n",
        s"val solution = csp.releasePlan(releaseMethod).$problemType($entity/$attribute)\n"
//        s"val solution = CSPproblem.$problemType($entity/$attribute)\n"
      )
    }

    def resetState: Callback = $.setState(State("",None, standardList))

    def setNewAttribute(attribute: Option[Attribute]): Callback = $.modState(_.copy(newAttribute = attribute))

    def sendMethod(P: Props, S: State): Callback = P.sendMethod(prepRelease(S, P.currentModel.makeString)) >> onClose(P)

    def onClose(P: Props): Callback = P.onClose() >> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)
      } else
        Callback()
    }
  }


    val component = ReactComponentB[Props]("Modal")
      .initialState(State("", None, standardList))
      .renderBackend[Backend]
      .build


    def apply(isOpen: Boolean, onClose: () => Callback, sendMethod: Seq[String] => Callback , currentModel: Tree)
    = component.set()(Props(isOpen, onClose, sendMethod, currentModel))

}
