package Data

import Utils.ConversionUtils._
import Data.Gamma._

abstract class Core {
    def toValue(implicit gamma: Gamma): Value.DelayedValue =
        Value.DelayedValue(this, gamma)
        
    def toValueImpl(implicit gamma: Gamma): Value.InstantValue

}

package object Core {
    // abstraction (function)
    case class λ(name: String, ty: Core, body: Core) extends Core {
        // override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
        //     Value.Closure(name, ty, body, gamma)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            (gamma find name) match {
                case Some((ty, Some(value))) => value
                case Some((ty, None)) =>
                    Value.Neut(Value.NeutVar(name, ty))
                case _ => throw new Error("couldn't find variable")
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            (closure.toValue.forced, param.toValue.forced) match {
                case (Value.Neut(neutral), value) =>
                    Value.Neut(Value.NeutApp(neutral, value))
                case (Value.Closure(name, ty, body, gamma1), value) =>
                    body.toValue(Def(name, ty, value)::gamma1)
                case _ => throw new Error("apply parameters to non-functions")
            }
    }
    // object App {
    //     def apply(closure: Core, paramA: Core, paramB: Core, params: Core*): App =
    //         params.foldLeft(App(App(closure, paramA), paramB))((fun, param) => App(fun, param))
    // }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Car(pair: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            pair.toValue.forced match {
                case Value.Cons(a, _) => a
                case Value.Neut(neut) => Value.Neut(Value.NeutCar(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            pair.toValue.forced match {
                case Value.Cons(a, d @ Value.Closure(name, ty, body, gamma)) =>
                    body.toValue(Def(name, ty, a) :: gamma)
                case Value.Neut(neut) => Value.Neut(Value.NeutCdr(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    case object U extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.U
    }

    case object Sole extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Sole
    }

    case object Zero extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Zero
    }

    case class Add1(inner: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Add1(inner.toValue)
    }

    case class Cons(a: Core, d: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Cons(a.toValue, d.toValue)
    }

    case class Π(name: String, ty: Core, body: Core, gamma: Gamma) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            Value.Π(name, ty.toValue, body, gamma)
    }

    case class Σ(name: String, ty: Core, body: Core, gamma: Gamma) extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue = 
            Value.Σ(name, ty.toValue, body, gamma)
    } 

    case object Trivial extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Trivial
    }

    case object Absurd extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.Absurd
    }

    case object ℕ extends Core {
        override def toValueImpl(implicit gamma: Gamma): Value.InstantValue =
            Value.ℕ
    }
    val Nat = ℕ
}