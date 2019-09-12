package Data
import Data.Core._
import Data.Gamma._
import Data.Src._
import Utils.ConversionUtils._

sealed abstract class Value {
    def forced: Value.InstantValue
}
package object Value {
    case class DelayedValue(core: Core, gamma: Gamma) extends Value {
        var value: Option[InstantValue] = None
        override def forced: InstantValue = value match {
            case None => {
                val actual: InstantValue = core.toValueImpl(gamma)
                value = Some(actual)
                actual
            }
            case Some(value) => value
        }   
    }

    sealed abstract class InstantValue extends Value {
        override def forced = this
        def unify(that: InstantValue)(implicit r: Renaming = Renaming.initial) =
            Op.Unify.unify(this, that)(r)
    }
    trait TermConstr extends InstantValue
    trait TypeConstr extends InstantValue

    trait ClosureLike extends InstantValue {
        val name: String
        val ty: Value
        val body: Core
        val gamma: Gamma
        var typical: Option[Value] = None
        def selfEval: InstantValue = typical match {
            case None => {
                val actual = body.toValue(Free(name, ty)::gamma)
                typical = Some(actual)
                actual
            }
            case Some(actual) => actual
        }
    }
    case class Closure(
        override val name: String, override val ty: Value,
        override val body: Core, override val gamma: Gamma
    ) extends InstantValue with TermConstr with ClosureLike
    case object U extends InstantValue
    case object Sole extends InstantValue with TermConstr
    case object Zero extends InstantValue with TermConstr
    case class Add1(inner: Value) extends InstantValue with TermConstr

    case class Cons(a: Value, d: Value) extends InstantValue with TermConstr

    // Π (x: A) -> B
    case class Π(
        override val name: String, override val ty: Value,
        override val body: Core, override val gamma: Gamma
    ) extends InstantValue with TypeConstr with ClosureLike
    case class Σ(
        override val name: String, override val ty: Value,
        override val body: Core, override val gamma: Gamma
    ) extends InstantValue with TypeConstr with ClosureLike
    case object Trivial extends InstantValue with TypeConstr
    case object Absurd extends InstantValue with TypeConstr
    case object ℕ extends InstantValue with TypeConstr
    case class Neut(neutral: Neutral) extends InstantValue

    sealed abstract class Neutral {
        def unify(that: Neutral)(implicit r: Renaming): Option[ErrorInfo] = Op.Unify.unify(this, that)
    }
    case class NeutVar(name: String, ty: Value) extends Neutral
    case class NeutApp(closure: Neutral, param: Value) extends Neutral
    case class NeutCar(pair: Neutral) extends Neutral
    case class NeutCdr(pair: Neutral) extends Neutral


    type Pi = Π; val Pi = Π
    type Sigma = Σ; val Sigma = Σ
    val Nat = ℕ;
}