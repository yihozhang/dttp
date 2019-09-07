import Gamma.Gamma
import Value._
import Utils._
package object Src {
    sealed abstract class Src {
        def toCore(implicit gamma: Gamma[Core]): Core
    }
    case class →(fr: Src, to: Src) extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core = Π(Utils.fresh, fr.toCore, to.toCore)
    }
    type Arrow = →; val Arrow = →
    case class Pair(fr: Src, to: Src) extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core = Σ(Utils.fresh, fr.toCore, to.toCore)
    }

    trait Core extends Src {
        override def toCore(implicit gamma: Gamma[Core]): Core = this
        def toValue(implicit gamma: Gamma[Core]): Value
    
    }
    // only core things can be values
    trait Value extends Core {
        override def toValue(implicit gamma: Gamma[Core]): Value = this
    }
    
    
    // below are Srcs

    // variable
    case class Var(name: String) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): Value = (gamma find name) match {
            case Some(value) => value.toValue
            case None => throw new Error("couldn't find variables in context")
        }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): Value = closure.toValue match {
            case Closure(name, out, gamma1) => out.toValue((name, param)::gamma1)
            case _ => throw new Error("apply parameters to non-functions")
        }
    }
    object App {
        def apply(closure: Core, param: Core, params: Core*): App = {
            params.foldLeft(App(closure, param))((fun, param) => App(fun, param))
        }
    }
    // abstraction (function)
    case class Abs(name: String, tp: Core, body: Core) extends Core {
        override def toValue(implicit gamma: Gamma[Core]): Value = Closure(name, body, gamma)
    }
    // closure
    case class Closure(name: String, body: Core, gamma: Gamma[Core]) extends Value
    case class Sole() extends Src with Value

    // below are types
    case class Π(name: String, fr: Core, to: Core) extends Src with Value
    type Pi = Π; val Pi = Π
    case class Σ(name: String, fr: Core, to: Core) extends Src with Value
    type Sigma = Σ; val Sigma = Σ 
    case class Unit() extends Src with Value
    case class Absurd() extends Src with Value
}