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
    trait Neutrallable extends Core // actually, Neutrallble is Var + TermElim
    // abstraction (function)
    case class λ(name: String, ty: Core, body: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue =
            Closure(name, ty.toValue, body, gamma)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            (gamma findName name) match {
                case Some(value) => value
                case None => Neut(this)
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            closure.toValue.forced match {
                case Closure(name, ty, out, gamma1) =>
                    out.toValue(Def(name, ty, param.toValue(gamma))::gamma1) // TODO: reconsider: actually need to check the type of param and the type of ty
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
    case class Car(pair: Core) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => a
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Core) extends Core with Neutrallable {
        override def toValueImpl(implicit gamma: Gamma): InstantValue = 
            pair.toValue.forced match {
                case Cons(a, b) => b
                case _ => throw new Error("apply car to non-pairs")
            }
    }
}