package Op
import Data._
import Data.Core
import Data.Value
import Data.Src
import Data.Gamma._
import Data.ErrorInfo
import Utils.ConversionUtils._
object Check {
    def check(src: Src, ty: Value)(implicit Γ: Gamma): Either[ErrorInfo, Core] = (src, ty) match {
        case (Src.Cons(loc, a, d), sigma @ Value.Σ(name, ty, body, gamma)) => for {
            a_o <- check(a, ty)
            d_o <- check(d, sigma.selfEval)
        } yield Core.Cons(a_o, d_o)
            
        case (Src.λ(loc, name, ty, body), pi @ Value.Π(_, _, _, _)) => for {
            ty_o <- check(ty, Value.U)
            _ <- (ty_o.toValue.forced unify pi.ty).convert
            body_o <- check(body, pi.selfEval)(Free(name, pi.ty)::Γ)
        } yield Core.λ(name, ty_o, body_o)
        // case Src.Zero(loc) => ???
        // case Src.Absurd(loc) => ???
        // case Src.Cdr(loc, pair) => ???
        // case Src.U(loc) => ???
        // case Src.Trivial(loc) => ???
        // case Src.Π(loc, name, ty, body) => ???
        // case Src.App(loc, closure, params) => ???
        // case Src.Sole(loc) => ???
        // case Src.ℕ(loc) => ???
        // case Src.Σ(loc, name, ty, body) => ???
        // case Src.Car(loc, pair) => ???
        // case Src.Add1(loc, inner) => ???
        // case Src.Var(loc, name) => ???
    }
}