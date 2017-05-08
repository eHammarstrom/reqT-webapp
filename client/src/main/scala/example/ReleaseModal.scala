package example

import japgolly.scalajs.react.vdom.prefix_<^.{<, ^, _}
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactEventI, ReactEvent, ReactKeyboardEventI, _}
import org.scalajs.dom.ext.KeyCode


object ReleaseModal {

  def modalStyle = Seq(
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

  case class State(newEntity: Option[Entity], newAttribute: Option[Attribute[Any]], sortBy: Seq[String], releaseType: ReleaseType = MAX)

  case class Props(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => String) => Option[Callback], currentModel: TreeItem)

  class Backend($: BackendScope[Props, State]) {
    def render(P: Props, S: State) =
      if (P.isOpen) {
        <.div(
          ^.onKeyDown ==> handleKeyDown(P, S),
          <.div(
            modalStyle,
            ^.width:= "400px",
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
              ),
              <.br,
              <.hr,
              <.br,
              <.dt(
                ^.textAlign.center,
                "sorted in order:"
              ),
              <.dd(
                orderSelect(0),
                orderSelect(1),
                orderSelect(2),
                orderSelect(3)
              )
            ),
            <.div(
              ^.width:= "95%",
              ^.padding := "20px",
              ^.display.flex,
              ^.justifyContent.spaceBetween,
              <.button("Cancel", ^.className := "btn btn-default pull-right", ^.bottom := "0px", ^.onClick ==> onClose(P)),
              <.button("OK", ^.className := "btn btn-success pull-right", ^.bottom := "0px", ^.onClick ==> send(P,S))
            )),
          <.div(
            backdropStyle,
            ^.onClick ==> onClose(P)
          )
        )
      } else
        <.div()


    val orderSelect = ReactComponentB[Int]("orderSelect")
      .render($ =>
        <.select(
          selectStyle(),
          ^.defaultValue := standardList($.props),
          ^.onChange ==> changeOrder($.props)
        )(
          <.option("Release"),
          <.option("Feature"),
          <.option("Stakeholder"),
          <.option("Resource")
        )
      )
      .build


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
      P.currentModel.children.map(_ => print(_))

      Seq(<.option("hej"))
    }



    def changeEntity(e: ReactEventI): Callback = {
      Callback(e.target.value)
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



    def prepRelease(state: State, model: String): String = {
      val entity = state.newEntity.getOrElse(Release()).toString.split('(').head
      val attribute = state.newAttribute.getOrElse(Benefit()).toString.split('(').head
      val problemType = state.releaseType match {
        case MAX => "maximize"
        case MIN => "minimize"
      }
      println(state.sortBy)


      val dollarMethod = "\n val problem = csp.releasePlan(m)\n" +
        s"val solution = problem.$problemType($attribute/$entity)\n" +
        s"val sortedSolution = solution.sortByTypes(${state.sortBy(0)}, ${state.sortBy(1)}, ${state.sortBy(2)}, ${state.sortBy(3)})\n" +
        "sortedSolution"

      "val m="+model+dollarMethod

    }

    def resetState: Callback = $.setState(State(None,None, standardList))
    def setNewEntity(entity: Option[Entity]): Callback = $.modState(_.copy(newEntity = entity))
    def setNewAttribute(attribute: Option[Attribute[Any]]): Callback = $.modState(_.copy(newAttribute = attribute))


    def send(P: Props, S: State)(e: ReactEvent): Callback =
      P.send(prepRelease(state = S, _)) match {
        case Some(callback) => callback >> onClose(P)(e)
        case None => Callback()
      }

    def onClose(P: Props)(e: ReactEvent): Callback = P.onClose(e) >> resetState

    def handleKeyDown(P: Props, S: State)(e: ReactKeyboardEventI): Callback = {
      if (e.nativeEvent.keyCode == KeyCode.Escape) {
        onClose(P)(e)
      } else
        Callback()
    }
  }



    val component = ReactComponentB[Props]("Modal")
      .initialState(State(None, None, standardList))
      .renderBackend[Backend]
      .build


    def apply(isOpen: Boolean, onClose: ReactEvent => Callback, send: (String => String) => Option[Callback], currentModel: TreeItem)
    = component.set()(Props(isOpen, onClose, send, currentModel))

}
