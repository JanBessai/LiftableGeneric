package shapeless.liftable
import scala.reflect.api.Universe
import shapeless._

trait Liftables {
  val universe: Universe
  import universe._

  trait GenericLiftable[T] {
    def liftable: Liftable[T]
  }

  trait AlgebraicInstances {

    implicit final def liftHCons[H, T <: HList](implicit
      lh: Cached[GenericLiftable[H]],
      lt: Cached[GenericLiftable[T]],
      htag: WeakTypeTag[H],
      ttag: WeakTypeTag[T]): GenericLiftable[H :: T] =
    new GenericLiftable[H::T] {
      val liftable: Liftable[H::T] =
        Liftable[H :: T] { (x: H :: T) =>
          q"""_root_.shapeless.::[${htag.tpe}, ${ttag.tpe}](${lh.value.liftable(x.head)}, ${lt.value.liftable(x.tail)})"""
        }
    }

    implicit val liftHNil: GenericLiftable[HNil] = new GenericLiftable[HNil] {
      def liftable: Liftable[HNil] =
        Liftable[HNil] { (x: HNil) =>
          q"_root_.shapeless.HNil"
        }
    }

    implicit final def liftCCons[L, R <: Coproduct](implicit
      ll: Cached[GenericLiftable[L]],
      lr: Cached[GenericLiftable[R]],
      ltag: WeakTypeTag[L],
      rtag: WeakTypeTag[R]): GenericLiftable[L :+: R] =
      new GenericLiftable[L :+: R] {
        val liftable: Liftable[L :+: R] =
          Liftable[L :+: R] {
            case Inl(l) => q"""_root_.shapeless.Inl[${ltag.tpe}, ${rtag.tpe}](${ll.value.liftable(l)})"""
            case Inr(r) => q"""_root_.shapeless.Inr[${ltag.tpe}, ${rtag.tpe}](${lr.value.liftable(r)})"""
          }
      }

    implicit val liftCNil = new GenericLiftable[CNil] {
      val liftable: Liftable[CNil] =
        Liftable[CNil] { (x: CNil) =>
          q"_root_.shapeless.CNil"
        }
    }
  }


  trait GenericInstances extends AlgebraicInstances {
    implicit final def liftGeneric[T, Repr](implicit
      gen: Generic.Aux[T, Repr],
      lrepr: Cached[Lazy[GenericLiftable[Repr]]],
      tag: WeakTypeTag[T]): GenericLiftable[T] =
      new GenericLiftable[T] {
        val liftable: Liftable[T] =
          Liftable[T] { (x: T) =>
            q"""_root_.shapeless.Generic[${tag.tpe}].from(${lrepr.value.value.liftable(gen.to(x))})"""
          }
      }
  }

  trait StandardInstances extends GenericInstances {
    implicit final def liftStandard[T](implicit
      lift: Liftable[T]): GenericLiftable[T] = new GenericLiftable[T] {
      val liftable = lift
    }
  }

  object GenericLiftable extends StandardInstances {
    def apply[T](implicit lt: GenericLiftable[T]): Liftable[T] = lt.liftable
  }

}

trait RuntimeLiftables extends Liftables {
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
}