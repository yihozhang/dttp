import Core.Neutrallable
import Core.Core
import Gamma.{Gamma, Free, Def}

package object Value {
    sealed abstract class Value {
        def forced: InstantValue
    }
    object Value {
        implicit def force(value: Value): InstantValue = value.forced
    }
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
        override val forced = this
        def describes(term: Value): Option[ErrorInfo]
        def unify(that: Value): Option[ErrorInfo] = ???
    }
    trait TermConstr extends InstantValue {
        override def describes(term: Value): Option[ErrorInfo]
            = Some(new ErrorInfo)
    }
    trait TypeConstr extends InstantValue

    case class Closure(name: String, ty: Value, body: Core, gamma: Gamma) extends InstantValue with TermConstr
    case object Sole extends InstantValue with TermConstr
    case object Zero extends InstantValue with TermConstr
    case class Add1(inner: Value) extends InstantValue with TermConstr
    case class Cons(a: Value, d: Value) extends InstantValue with TermConstr

    // Π (x: A) -> B
    case class Π(name: String, fr: Value, to: Value) extends InstantValue with TypeConstr {
        override def describes(term: Value): Option[ErrorInfo] =
            term match {
                case Closure(name, ty, body, gamma1) => {
                    (fr unify ty).orElse {
                        to.describes(body.toValue(Free(name, ty)::gamma1))
                    }
                }   
                case _ => Some(new ErrorInfo)
            }
    }

    // Σ (A, D)
    type Pi = Π; val Pi = Π
    case class Σ(name: String, fr: Value, to: Value) extends InstantValue with TypeConstr {
        override def describes(term: Value): Option[ErrorInfo] =
            term match {
                case Cons(a, Closure(name, ty, body, gamma)) => {
                    fr.describes(a)
                        .orElse(to.describes(body.toValue(Def(name, ty, a)::gamma)))
                }
                case _ => Some(new ErrorInfo)
            } 
    }
    type Sigma = Σ; val Sigma = Σ 
    case object Trivial extends InstantValue with TypeConstr {
        override def describes(term: Value): Option[ErrorInfo] =
            term match {
                case Sole => None
                case _ => Some(new ErrorInfo)
            }
    }
    case object Absurd extends InstantValue with TypeConstr {
        override def describes(term: Value): Option[ErrorInfo] =
            Some(new ErrorInfo)
    }
    case object ℕ extends InstantValue with TypeConstr {
        override def describes(term: Value): Option[ErrorInfo] = {
            term match {
                case Zero => None
                case Add1(inner) => ℕ.describes(inner)
                case _ => Some(new ErrorInfo)
            }
        }
    }
    val Nat = ℕ;
    case class Neut(neut: Neutrallable) extends InstantValue {
        override def describes(term: Value): Option[ErrorInfo] = {
            None // TODO: this
        }
    }
    object Neut {
        sealed abstract class Neutral
        // case class NeutTermElim(elim: Elim) extends Neutral
        // case class NeutVar(ref: Var) extends Neutral
    }
}