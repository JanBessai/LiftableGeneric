import shapeless._
import nat._; import ops.nat.Pred

trait RangeAlt[A <: Nat, B <: Nat] extends DepFn0 with Serializable {
  type Out <: HList
}

trait DoSucc {
  type Aux[A <: Nat, B <: Nat, Out0 <: HList] = RangeAlt[A, B] {type Out = Out0}
  implicit def range2[A <: Nat, B <: Nat, L <: HList]
  (implicit
    w: Witness.Aux[A],
    r: Lazy[Aux[Succ[A], B, L]]
  ): Aux[A, B, A :: L] =
    new RangeAlt[A, B] {
      type Out = A :: L

      def apply(): Out =  w.value :: r.value()
    }
}

trait DoStop extends DoSucc {
  import shapeless.ops.hlist._

  implicit def range1[A <: Nat]: Aux[A, A, HNil] =
    new RangeAlt[A, A] {
      type Out = HNil

      def apply(): Out = HNil
    }
}

object RangeAlt extends DoStop {
  def apply[A <: Nat, B <: Nat](implicit range: RangeAlt[A, B]): Aux[A, B, range.Out] = range
}

object Test extends App {
  val rt = the[RangeAlt[_1, _3]]
  println(rt.apply())
}