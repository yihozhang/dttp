import Gamma._
import Utils._
import Value._

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
    object Core {
        implicit def value(core: Core)(implicit gamma: Gamma) = core.toValue
    }
    // abstraction (function)
    case class λ(name: String, ty: Core, body: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue =
            Closure(name, ty, body, gamma)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            (gamma find name) match {
                case Some((ty, Some(value))) => value.forced
                case Some((ty, None)) => Neut(Neut.NeutVar(name, ty))
                case _ => throw new Error("couldn't find variable")
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            (closure.toValue.forced, param.toValue.forced) match {
                case (closure @ Closure(name, ty, body, gamma1), Neut(neut)) =>
                    Neut(Neut.NeutApp(closure, neut))
                case (Closure(name, ty, body, gamma1), value) =>
                    body.toValue(Def(name, ty, value)::gamma1) // TODO: reconsider: actually need to check the type of param and the type of ty
                case _ => throw new Error("apply parameters to non-functions")
            }
    }
    object App {
        def apply(closure: Core, paramA: Core, paramB: Core, params: Core*): App =
            params.foldLeft(App(App(closure, paramA), paramB))((fun, param) => App(fun, param))
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Car(pair: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => a
                case Neut(neut) => Neut(Neut.NeutCar(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => b
                case Neut(neut) => Neut(Neut.NeutCdr(neut))
                case _ => throw new Error("apply car to non-pairs")
            }
    }
}