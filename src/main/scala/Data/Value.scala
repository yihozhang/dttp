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
        def readback: Core = ???
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
    }

    sealed abstract class InstantValue extends Value {
        override def forced = this
    }
    trait TermConstr extends InstantValue
    trait TypeConstr extends InstantValue

    trait ClosureLike extends InstantValue {
        val name: String
        val ty: Value
        val body: Core
        val ρ: Env
        var typical: Option[Value] = None
        def selfEval: InstantValue = typical match {
            case None => {
                val actual = body.toValue((name -> ty)::ρ)
                typical = Some(actual)
                actual.forced
            }
            case Some(actual) => actual.forced
        }
    }
    case class Closure(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with TermConstr with ClosureLike
    case object U extends InstantValue
    case object Sole extends InstantValue with TermConstr
    case object Zero extends InstantValue with TermConstr
    case class Add1(inner: Value) extends InstantValue with TermConstr

    case class Cons(a: Value, d: Value) extends InstantValue with TermConstr

    // Π (x: A) -> B
    case class Π(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with TypeConstr with ClosureLike
    case class Σ(
        override val name: String, override val ty: Value,
        override val body: Core, override val ρ: Env
    ) extends InstantValue with TypeConstr with ClosureLike
    case object Trivial extends InstantValue with TypeConstr
    case object Absurd extends InstantValue with TypeConstr
    case object ℕ extends InstantValue with TypeConstr
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