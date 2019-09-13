package Op
import Data.Src.Src
import Data.Src
import Data.Core.Core
import Data.Gamma._
import Data.Core
import Data.Core.Core
import Data.Result._
import Data.Value
import Data.Value.Value
import Op.Check.check
package object Infer {
    def infer(src: Src)(implicit Γ: Gamma): Result[(Core, Value)] = {
        implicit val ρ = Γ.toEnv
        src match {
            case Src.Add1(loc, inner) => for {
                inner_o <- check(inner, Value.Nat)
            } yield (Core.Add1(inner_o), Value.Nat)
            case Src.Var(loc, name) => (Γ findType name) match {
                case Some(value) => Exact((Core.Var(name), value))
                case None => ErrorInfo(s"couldn't find variable $name")
            }
            case src @ (
                Src.Π(_,_,_,_)
                | Src.Absurd(_)
                | Src.Trivial(_)
                | Src.Σ(_, _, _, _)
                | Src.ℕ(_)
                | Src.→(_, _, _, _)
            ) => for {
                src_o <- check(src, Value.U)
            } yield (src_o, Value.U)
            case Src.Sole(loc) => Exact(Core.Sole, Value.Trivial)
            case Src.Car(loc, pair) => for {
                out <- infer(pair)
                (pair_o, pair_ty_o) = out
                ty_o <- pair_ty_o match {
                    case pair_o @ Value.Σ(name, ty, body, gamma) => Exact(ty)
                    case _ => ErrorInfo("applying Car to non-Sigma type")
                }
            } yield (Core.Car(pair_o), ty_o)
            case Src.Cdr(loc, pair) => for {
                out <- infer(pair)
                (pair_o, pair_ty_o) = out
                ty_o <- pair_ty_o match {
                    case pair_o @ Value.Σ(name, ty, body, gamma) => Exact(pair_o.selfEval)
                    case _ => ErrorInfo("applying Cdr to non-Sigma type")
                }
            } yield (Core.Cdr(pair_o), ty_o)
            case Src.App(loc, closure, param) => for {
                out <- infer(closure)
                (closure_o, closure_ty_o) = out
                out <- closure_ty_o match {
                    case closure_ty_o @ Value.Π(name, ty, body, ρ) => for {
                        param_o <- check(param, ty)
                    } yield (param_o, closure_ty_o.selfEval)
                    case _ => ErrorInfo("applying parameters to non-functions")
                }
                (param_o, ty) = out
            } yield (Core.App(closure_o, param_o), ty)
            case Src.Zero(loc) => Exact((Core.Zero, Value.ℕ))
            case Src.U(loc) => ErrorInfo("every type is a U, but U has no types")
            case Src.λ(_, _, _) | Src.Cons(_, _, _) => ErrorInfo("can't infer type for lambda and cons")
        }
    }
}