import Gamma.Gamma
import Utils._
package object Src {
    type ValueResult = (Value, Gamma[Core])
    sealed abstract class Src {
        def toCore(implicit gamma: Gamma[Core]): Core
    }
    case class →(fr: Src, to: Src) extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core =
            Π(Utils.fresh, fr.toCore, to.toCore)
    }
    type Arrow = →; val Arrow = →
    case class Pair(fr: Src, to: Src) extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core = 
            Σ(Utils.fresh, fr.toCore, to.toCore)
    }

    abstract class Core extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core = this
        def toValue(implicit gamma: Gamma[Core]): ValueResult
        // def describes(term: Core): 
    }
    // abstraction (function)
    case class Abs(name: String, tp: Core, body: Core) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): ValueResult =
            (Closure(name, body, gamma), gamma)
    }
    // variable
    case class Var(name: String) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): ValueResult = 
            (gamma find name) match {
                case Some(value) => value.toValue
                case None => throw new Error("couldn't find variables in context")
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): ValueResult = 
            closure.toValue match {
                case (Closure(name, out, gamma1), _) => out.toValue((name, param)::gamma1)
                case _ => throw new Error("apply parameters to non-functions")
            }
    }
    object App {
        def apply(fun: Core, param: Core, params: Core*): App =
            params.foldLeft(App(fun, param))((fun, param) => App(fun, param))
    }

    // only core things can be values
    abstract class Value extends Core {
        override def toValue(implicit gamma: Gamma[Core]): ValueResult =
            (this, gamma)
    }

    // closure
    case class Closure(name: String, body: Core, gamma: Gamma[Core]) extends Value
    case object Sole extends Value
    case object Zero extends Value
    case class Add1(inner: Core) extends Value
    // below are types
    case class Π(name: String, fr: Core, to: Core) extends Value
    type Pi = Π; val Pi = Π
    case class Σ(name: String, fr: Core, to: Core) extends Value
    type Sigma = Σ; val Sigma = Σ 
    case object Unit extends Value
    case object Absurd extends Value
    case object Nat extends Value
}