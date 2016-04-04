
object data {
  case class Foo()
}


object ImplicitTesting extends App {
  import data._
  import shapeless._

  // works
  val x = Generic[List[Int]]

  // Setup
  import scala.tools.reflect._
  val universe = scala.reflect.runtime.currentMirror.universe
  val tb = scala.reflect.runtime.currentMirror.mkToolBox(frontEnd = mkConsoleFrontEnd(-1), options="-language:experimental.macros -Ydebug -Ymacro-debug-verbose -Xlog-implicits")
  import universe._
  /*val t = q"import _root_.shapeless._; Generic[data.Foo]"

  val tt = tb.typecheck(t, tb.TERMmode)*/
  println(showRaw(tb.inferImplicitValue(typeOf[Generic[data.Foo]], silent = false, withMacrosDisabled = false)))


}
