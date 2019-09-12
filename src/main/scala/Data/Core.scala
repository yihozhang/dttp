package Data

// import Utils.ConversionUtils._
import Data.Gamma.Env

package object Core {
    abstract class Core {
        def toValue(implicit ρ: Env): Value.DelayedValue =
            Value.DelayedValue(this, ρ)
            
        def toValueImpl(implicit ρ: Env): Value.InstantValue
    
    }
    // abstraction (function)
    case class λ(name: String, ty: Core, body: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Closure(name, ty.toValue, body, ρ)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            (ρ find name) match {
                case Some(value) => value.forced
                case _ => throw new Error("couldn't find variable")
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            (closure.toValue.forced, param.toValue.forced) match {
                case (Value.Neut(neutral), value) =>
                    Value.Neut(Value.NeutApp(neutral, value))
                case (Value.Closure(name, ty, body, ρ1), value) =>
                    body.toValue((name -> value)::ρ1).forced
                case _ => throw new Error("apply parameters to non-functions")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Car(pair: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            pair.toValue.forced match {
                case Value.Cons(a, _) => a.forced
                case Value.Neut(neut) => Value.Neut(Value.NeutCar(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            pair.toValue.forced match {
                case Value.Cons(a, d @ Value.Closure(name, ty, body, ρ)) =>
                    body.toValue((name -> a) :: ρ).forced
                case Value.Neut(neut) => Value.Neut(Value.NeutCdr(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    case object U extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.U
    }

    case object Sole extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Sole
    }

    case object Zero extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Zero
    }

    case class Add1(inner: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Add1(inner.toValue)
    }

    case class Cons(a: Core, d: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Cons(a.toValue, d.toValue)
    }

    case class Π(name: String, ty: Core, body: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            Value.Π(name, ty.toValue, body, ρ)
    }

    case class Σ(name: String, ty: Core, body: Core) extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue = 
            Value.Σ(name, ty.toValue, body, ρ)
    } 

    case object Trivial extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Trivial
    }

    case object Absurd extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.Absurd
    }

    case object ℕ extends Core {
        override def toValueImpl(implicit ρ: Env): Value.InstantValue =
            Value.ℕ
    }
    val Nat = ℕ
}