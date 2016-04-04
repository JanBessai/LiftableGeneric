
object data {
  case class Foo()
}




object ImplicitTesting extends App {
  //
  import data._

  val x = _root_.shapeless.Generic[Foo]

  import scala.tools.reflect._

  val universe = scala.reflect.runtime.currentMirror.universe
  val tb = scala.reflect.runtime.currentMirror.mkToolBox()
  import universe._
  val tag = implicitly[TypeTag[Foo]]

  val tag2 = implicitly[TypeTag[_root_.shapeless.Generic[Foo]]]
  println(showRaw(tq"${tag2.tpe.typeSymbol}"))
  tb.eval(q"import _root_.shapeless._; Generic[${tag.tpe.dealias.typeSymbol}]").asInstanceOf[_root_.shapeless.Generic[Foo]]
}
