package Op
import Data.Src.Src
import Data.Src
import Data.Core.Core
import Data.Gamma.Gamma
import Data.Core
import Data.Core.Core
import Data.Result._
import Data.Value
import Data.Value.Value
import Op.Check.check
package object Infer {
    def infer(src: Src)(implicit Γ: Gamma): Result[(Core, Value)] = src match {
        case Src.Add1(loc, inner) => for {
            inner_o <- check(inner, Value.Nat)
        } yield (Core.Add1(inner_o), Value.Nat)
        case Src.Var(loc, name) => (Γ findType name) match {
            case Some(value) => Exact(???)
            case None => ErrorInfo(s"couldn't find variable $name")
        }
        // case Src.Π(loc, name, ty, body) =>
        // case Src.Absurd(loc) =>
        // case Src.Trivial(loc) =>
        // case Src.λ(loc, name, body) =>
        // case Src.Sole(loc) =>
        // case Src.Σ(loc, name, ty, body) =>
        // case Src.Cons(loc, a, d) =>
        // case Src.Cdr(loc, pair) =>
        // case Src.Car(loc, pair) =>
        // case Src.App(loc, closure, params) =>
        // case Src.ℕ(loc) =>
        // case Src.→(loc, name, a, b) =>
        // case Src.Zero(loc) =>
        // case Src.U(loc) =>
    }
}