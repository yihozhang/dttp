package Op
import Data._
import Data.Core
import Data.Value
import Data.Value.Value
import Data.Src
import Data.Src.Src
import Data.Gamma._
import Data.Result._
import Op.Infer.infer
import Data.Value.Π
import _root_.Utils.Conversion._
import _root_.Utils.Fresh._
object Check {

    def check(src: Src, ty: Value)(implicit Γ: Gamma): Result[Core.Core] = {
        implicit val ρ = Γ.toEnv
        (src, ty) match {
            case (Src.Cons(loc, a, d), sigma @ Value.Σ(name, ty, body, gamma)) => for {
                a_o <- check(a, ty)
                d_o <- check(d, body.toValue((name -> a_o.toValue)::ρ))
            } yield Core.Cons(a_o, d_o)     
            case (Src.λ(loc, name, body), pi @ Value.Π(_, _, _, _)) => for {
                body_o <- check(body, pi.selfEval)(Free(name, pi.ty)::Γ)
            } yield Core.λ(name, pi.ty.readback, body_o)
            case (Src.Zero(loc), Value.Nat) => Exact(Core.Zero)
            case (Src.Absurd(loc), Value.U) => Exact(Core.U)
            case (Src.Car(loc, pair), ty) =>
            for {
                out <- infer(pair);
                (pair_o, pair_ty_o) = out
                _ <- (pair_ty_o match {
                    case pair_ty_o @ Value.Σ(_, _, _, _) =>
                        (pair_ty_o.ty unify ty)
                    case _ => ErrorInfo(s"expected $ty but get ${out._2}")
                })
            } yield Core.Car(pair_o)
            case (Src.Cdr(loc, pair), ty) => for {
                out <- infer(pair)
                (pair_o, pair_ty_o) = out
                _ <- (pair_ty_o match {
                    case pair_ty_o @ Value.Σ(_, _, _, _) => {
                        (pair_ty_o.selfEval unify ty)
                    }
                    case _ => ErrorInfo(s"expected $ty but get $pair_ty_o")
                })
            } yield Core.Cdr(pair_o)
            case (Src.U(loc), _) => ErrorInfo("U describes a type, but U is not a type")
            case (Src.Trivial(loc), Value.U) => Exact(Core.Trivial)
            case (Src.→(loc, a, b), Value.U) => for {
                a_o <- check(a, Value.U)
                name = fresh
                b_o <- check(b, Value.U)(Free(name, a_o.toValue)::Γ)
            } yield Core.Π(name, a_o, b_o)
            case (Src.Pair(loc, a, b), Value.U) => for {
                a_o <- check(a, Value.U)
                name = fresh
                b_o <- check(b, Value.U)(Free(name, a_o.toValue)::Γ)
            } yield Core.Σ(name, a_o, b_o)
            case (Src.Π(loc, name, ty, body), Value.U) => for {
                ty_o <- check(ty, Value.U)
                body_o <- check(body, Value.U)(Free(name, ty_o.toValue)::Γ)
            } yield Core.Π(name, ty_o, body_o)
            case (Src.App(loc, closure, param), ty) => for {
                out <- infer(closure)
                (closure_o, closure_ty_o) = out
                param_o <- closure_ty_o.forced match {
                    case Π(name, ty, body, gamma) => check(param, ty)
                    case _ => ErrorInfo()
                }
            } yield Core.App(closure_o, param_o)
            case (Src.Sole(loc), Value.Trivial) => Exact(Core.Sole)
            case (Src.ℕ(loc), Value.U) => Exact(Core.ℕ)
            case (Src.Σ(loc, name, ty, body), Value.U) => for {
                ty_o <- check(ty, Value.U)
                body_o <- check(body, Value.U)(Free(name, ty_o.toValue)::Γ)
            } yield Core.Σ(name, ty_o, body_o)
            case (Src.Add1(loc, inner), Value.Nat) => for {
                inner_o <- check(inner, Value.Nat)
            } yield Core.Add1(inner_o)
            case (Src.Var(loc, name), ty) => (Γ find name) match {
                case Some((var_ty, _)) => (var_ty unify ty) map ( _ => Core.Var(name))
                case None => ErrorInfo(s"couldn't find variable $name")
            }
            case _ => new ErrorInfo(s"expected $ty but get $src")
        }
    }
}