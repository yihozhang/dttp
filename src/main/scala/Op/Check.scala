package Op
import Data._
import Data.Core
import Data.Value
import Data.Value.Value
import Data.Src
import Data.Src.Src
import Data.Gamma._
import Data.ErrorInfo
import Op.Infer.infer
import Data.Value.Π
// import Utils.ConversionUtils._
object Check {

    class OptionConverter(option: Option[ErrorInfo]) {
        def convert: Either[ErrorInfo, Unit] = option match {
            case None => Right(Unit)
            case Some(value) => Left(value)
        }
    }
    implicit def convert(option: Option[ErrorInfo]) = new OptionConverter(option)
    def check(src: Src, ty: Value)(implicit Γ: Gamma): Either[ErrorInfo, Core.Core] = (src, ty) match {
        case (Src.Cons(loc, a, d), sigma @ Value.Σ(name, ty, body, gamma)) => for {
            a_o <- check(a, ty)
            d_o <- check(d, body.toValue(Def(name, ty, a_o.toValue)::gamma))
        } yield Core.Cons(a_o, d_o)     
        case (Src.λ(loc, name, ty, body), pi @ Value.Π(_, _, _, _)) => for {
            ty_o <- check(ty, Value.U)
            _ <- (ty_o.toValue.forced unify pi.ty.forced).convert
            body_o <- check(body, pi.selfEval)(Free(name, pi.ty)::Γ)
        } yield Core.λ(name, ty_o, body_o)
        case (Src.Zero(loc), Value.Nat) => Right(Core.Zero)
        case (Src.Absurd(loc), Value.U) => Right(Core.U)
        case (Src.Car(loc, pair), ty) => for {
            out <- infer(pair)
            (pair_o, pair_ty_o) = out
            _ <- (pair_ty_o match {
                case pair_ty_o @ Value.Σ(_, _, _, _) => {
                    (pair_ty_o.ty unify ty).convert
                }
                case _ => Left(new ErrorInfo(s"expected $ty but get $pair_ty_o"))
            })
        } yield Core.Car(pair_o)
        case (Src.Cdr(loc, pair), ty) => for {
            out <- infer(pair)
            (pair_o, pair_ty_o) = out
            _ <- (pair_ty_o match {
                case pair_ty_o @ Value.Σ(_, _, _, _) => {
                    (pair_ty_o.selfEval unify ty).convert
                }
                case _ => Left(new ErrorInfo(s"expected $ty but get $pair_ty_o"))
            })
        } yield Core.Cdr(pair_o)
        case (Src.U(loc), _) => Left(new ErrorInfo("U describes a type, but U is not a type"))
        case (Src.Trivial(loc), Value.U) => Right(Core.Trivial)
        case (Src.Π(loc, name, ty, body), Value.U) => for {
            ty_o <- check(ty, Value.U)
            body_o <- check(body, Value.U)(Free(name, ty_o.toValue)::Γ)
        } yield Core.Π(name, ty_o, body_o)
        case (Src.App(loc, closure, param), ty) => for {
            // _ <- if (params == 0) Left(new ErrorInfo) else Right()
            out <- infer(closure)
            (closure_o, closure_ty_o) = out
            param_o <- closure_ty_o.forced match {
                case Π(name, ty, body, gamma) => check(param, ty)
                case _ => Left(new ErrorInfo())
            }
        } yield Core.App(closure_o, param_o)
        case (Src.Sole(loc), Value.Trivial) => Right(Core.Sole)
        case (Src.ℕ(loc), Value.U) => Right(Core.ℕ)
        case (Src.Σ(loc, name, ty, body), Value.U) => for {
            ty_o <- check(ty, Value.U)
            body_o <- check(body, Value.U)(Free(name, ty_o.toValue)::Γ)
        } yield Core.Σ(name, ty_o, body_o)
        case (Src.Add1(loc, inner), Value.Nat) => for {
            inner_o <- check(inner, Value.Nat)
        } yield Core.Add1(inner_o)
        case (Src.Var(loc, name), ty) => (Γ find name) match {
            case Some((var_ty, _)) => (var_ty unify ty).convert map( _ => Core.Var(name))
            case None => Left(new ErrorInfo(s"couldn't find variable $name"))
        }
        case _ => Left(new ErrorInfo(s"expected $ty but get $src"))
    }
}