package shapeless.liftable.example.expression

import shapeless.liftable._

package data {
  sealed trait Exp
  case class Plus(x: Exp, y: Exp) extends Exp
  case class Const(x: Int) extends Exp
}



object ExpressionInstances extends App with RuntimeLiftables {
  import data._

  implicit val lift = GenericLiftable[Plus]
  val test: Plus = Plus(Const(21), Const(21))

  import scala.tools.reflect._
  val tb = scala.reflect.runtime.currentMirror.mkToolBox()
  import universe._

  println(tb.eval(q"${test}"))
}
