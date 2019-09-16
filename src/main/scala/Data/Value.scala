package Data
import Data.Core._
import Data.Gamma._
import Data.Src._
import Data.Result._
package object Value {
    sealed abstract class Value {
        def forced: InstantValue
        def unify(that: Value)(implicit r: Renaming = Renaming.initial): Result[Unit] =
            Op.Unify.unify(this.forced, that.forced)(r)
        def readback: Core = Op.Readback.readback(this)
    }
    case class DelayedValue(core: Core, ρ: Env) extends Value {
        var value: Option[InstantValue] = None
        override def forced: InstantValue = value match {
            case None => {
                val actual: InstantValue = core.toValueImpl(ρ)
                value = Some(actual)
                actual
            }
            case Some(value) => value
        }
        override def readback: Core = this.forced.readback
    }

    sealed abstract class InstantValue extends Value {
        override def forced = this
    }
    sealed trait ClosureLike extends InstantValue {
        val name: String
        val ty: Value
        val body: Core
        val ρ: Env
        var typical: Option[Value] = None
        def selfEval: InstantValue = typical match {
            case None => {
                val actual = body.toValue((name -> Neut(NeutVar(name, ty)))::ρ)
                typical = Some(actual)
                actual.forced
            }
            case Some(actual) => actual.forced
        }
    }
    case class Closure(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with ClosureLike
    case object U extends InstantValue
    case object Sole extends InstantValue
    case object Zero extends InstantValue
    case class Add1(inner: Value) extends InstantValue

    case class Cons(a: Value, d: Value) extends InstantValue

    // Π (x: A) -> B
    case class Π(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with ClosureLike
    case class Σ(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with ClosureLike
    case object Trivial extends InstantValue
    case object Absurd extends InstantValue
    case object ℕ extends InstantValue
    case class ≡(ty: Value, fr: Value, to: Value) extends InstantValue
    case class Same(value: Value) extends InstantValue
    case class Neut(neutral: Neutral) extends InstantValue

    sealed abstract class Neutral {
        def unify(that: Neutral)(implicit r: Renaming): Result[Unit] = Op.Unify.unify(this, that)
    }
    case class NeutVar(name: String, ty: Value) extends Neutral
    case class NeutApp(closure: Neutral, param: Value) extends Neutral
    case class NeutCar(pair: Neutral) extends Neutral
    case class NeutCdr(pair: Neutral) extends Neutral


    type Pi = Π; val Pi = Π
    type Sigma = Σ; val Sigma = Σ
    val Nat = ℕ;
}