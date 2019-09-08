import Gamma._
import Utils._
package object Core {
    // type ValueResult = (Value, Gamma)
    // sealed abstract class Src {
    //     def toCore(implicit gamma: Gamma): Core
    // }
    // case class →(fr: Src, to: Src) extends Src {
    //     override def toCore(implicit gamma: Gamma): Core =
    //         Π(Utils.fresh, fr.toCore, to.toCore)
    // }
    // type Arrow = →; val Arrow = →
    // case class Pair(fr: Src, to: Src) extends Src {
    //     override def toCore(implicit gamma: Gamma): Core = 
    //         Σ(Utils.fresh, fr.toCore, to.toCore)
    // }

    abstract class Core {
        implicit def toValue(implicit gamma: Gamma): DelayedValue =
            DelayedValue(this, gamma)    
        def toValueImpl(implicit gamma: Gamma): InstantValue
    }
    trait Neutrallable extends Core // actually, Neutrallble is Var + TermElim
    // abstraction (function)
    case class λ(name: String, ty: Value, body: Value) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue =
            Closure(name, ty, body, gamma)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            (gamma findName name) match {
                case Some(value) => value.toValue
                case None => Neut(this)
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            closure.toValue.forced match {
                case Closure(name, ty, out, gamma1) =>
                    out.toValue(Def(name, ty, param.toValue(gamma))::gamma1)
                case _ => throw new Error("apply parameters to non-functions")
            }
    }
    object App {
        def apply(fun: Value, paramA: Value, paramB: Value, params: Value*): App =
            params.foldLeft(App(App(fun, paramA), paramB))((fun, param) => App(fun, param))
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Car(pair: Value) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => a
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Value) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => b
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    // only core things can be values
    sealed abstract class Value extends Core {
        def forced: InstantValue
        override def toValueImpl(implicit gamma: Gamma): InstantValue = this.forced
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