package shapeless.liftable.example.expression


package data {
  sealed trait Exp
  case class Plus(x: Exp, y: Exp) extends Exp
  case class Const(x: Int) extends Exp
  case class Foo() extends Exp
}

import shapeless._
import shapeless.liftable._

object ExpressionInstances extends App with liftables {
  val universe = scala.reflect.runtime.currentMirror.universe

  import data._

  //implicit val genPlus = Generic[data.Const]
  /*val liftHNil = runtimeLiftables.GenericLiftable[HNil]
  val liftGInt = runtimeLiftables.GenericLiftable.liftStandard[Int]*/
  val liftList = GenericLiftable[Foo]
  val test: Plus = Plus(Const(21), Const(21))

  import scala.tools.reflect._

  val tb = scala.reflect.runtime.currentMirror.mkToolBox()
  import universe._
  val tag = implicitly[WeakTypeTag[Foo]]
  println(tb.eval(q"${liftList.liftable(Foo())}"))



  /*trait Bar { def apply(implicit x: Generic[Foo]): Generic[Foo] }
  val barTag = implicitly[WeakTypeTag[Bar]]
  println(tb.eval(q"object X extends ${barTag.tpe} { def apply(implicit x : _root_.shapeless.Generic[${tag.tpe}]) = x }; X.apply").asInstanceOf[Foo])
  */

  // val x = $x; x.from[_root_.shapeless.::[Int,_root_.shapeless.HNil]](_root_.shapeless.::[Int, _root_.shapeless.HNil](42, _root_.shapeless.HNil))")) //${liftList.liftable(42)}"))

    //)

    /*implicit val genExp = Generic[Exp]
    implicit val tag: WeakTypeTag[Exp] = implicitly[WeakTypeTag[Exp]]*/
    //implicit val liftExp = GenericLiftable[Exp]


    //
    //val lift = GenericLiftable.liftHCons[Int, HNil]
    //val lplus = GenericLiftable[Plus](GenericLiftable.liftGeneric(genPlus, GenericLiftable.liftHCons, tag))
    //val testQ = q"$test"



}
