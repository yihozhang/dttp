import Ctx._
import Utils._
package object Src {
    type ValueResult = (Value, Ctx)
    sealed abstract class Src {
        def toCore(implicit ctx: Ctx): Core
    }
    case class →(fr: Src, to: Src) extends Src {
        override def toCore(implicit ctx: Ctx): Core =
            Π(Utils.fresh, fr.toCore, to.toCore)
    }
    type Arrow = →; val Arrow = →
    case class Pair(fr: Src, to: Src) extends Src {
        override def toCore(implicit ctx: Ctx): Core = 
            Σ(Utils.fresh, fr.toCore, to.toCore)
    }

    abstract class Core extends Src {
        override def toCore(implicit ctx: Ctx): Core = this
        def toValue(implicit ctx: Ctx): ValueResult
        def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] =
            this.toValue._1.describes(term)
    }
    // abstraction (function)
    case class λ(name: String, tp: Core, body: Core) extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult =
            (Closure(name, body, ctx), ctx)
    }
    type Abs = λ; val Abs = λ;
    // variable
    case class Var(name: String) extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult = 
            (ctx.naming find name) match {
                case Some(value) => value.toValue
                case None => throw new Error("couldn't find variables in context")
            }
    }
    // applcation
    case class App(closure: Core, param: Core) extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult = 
            closure.toValue match {
                case (Closure(name, out, ctx1), _) =>
                    out.toValue(ctx1.andNaming(name -> param))
                case _ => throw new Error("apply parameters to non-functions")
            }
    }
    object App {
        def apply(fun: Core, param: Core, params: Core*): App =
            params.foldLeft(App(fun, param))((fun, param) => App(fun, param))
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Car(pair: Core) extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult = 
            pair.toValue match {
                case (Cons(a, b), ctx1) => a.toValue(ctx1)
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    /**
     * Cdr is an eliminator for Cons
     */
    case class Cdr(pair: Core) extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult = 
            pair.toValue match {
                case (Cons(a, b), ctx1) => b.toValue(ctx1)
                case _ => throw new Error("apply car to non-pairs")
            }
    }

    // only core things can be values
    abstract class Value extends Core {
        override def toValue(implicit ctx: Ctx): ValueResult =
            (this, ctx)
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo]
    }
    trait TermConstr extends Value {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo]
            = Some(new ErrorInfo)
    }
    trait TypeConstr extends Value

    case class Closure(name: String, body: Core, ctx: Ctx) extends Value with TermConstr
    case object Sole extends Value with TermConstr
    case object Zero extends Value with TermConstr
    case class Add1(inner: Core) extends Value with TermConstr
    case class Cons(a: Core, d: Core) extends Value with TermConstr

    // Π (x: A) -> B
    case class Π(name: String, fr: Core, to: Core) extends Value with TypeConstr {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] =
            term.toValue match {
                case (Closure(name, body, ctx1), _) => {
                    val bodyo = body.toValue(ctx1.andTyping(name -> fr))
                    to.describes(bodyo._1)(bodyo._2)
                }   
                case _ => Some(new ErrorInfo)
            }
    }

    // Σ (A, D)
    type Pi = Π; val Pi = Π
    case class Σ(name: String, fr: Core, to: Core) extends Value with TypeConstr {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] =
            term.toValue match {
                case (Cons(a, d), ctx1) => {
                    fr.describes(a)(ctx1)
                        .orElse(to.describes(d)(ctx1.andTyping(name -> fr)))
                }
                case _ => Some(new ErrorInfo)
            } 
    }
    type Sigma = Σ; val Sigma = Σ 
    case object Unit extends Value with TypeConstr {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] =
            term.toValue match {
                case (Sole, _) => None
                case _ => Some(new ErrorInfo)
            }
    }
    case object Absurd extends Value with TypeConstr {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] =
            Some(new ErrorInfo)
    }
    case object ℕ extends Value with TypeConstr {
        override def describes(term: Core)(implicit ctx: Ctx): Option[ErrorInfo] = {
            println(ctx)
            println(term)
            term.toValue match {
                case (Zero, _) => None
                case (Add1(inner), ctx1) => ℕ.describes(inner)(ctx1)
                case _ => Some(new ErrorInfo)
            }
        }
    }
    val Nat = ℕ;
    case class Neut(uncaptured: Core)
}