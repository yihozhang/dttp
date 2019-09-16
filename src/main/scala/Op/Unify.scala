package Op

import Data.Value._
import Data.Gamma.Renaming
import Data.Result._

package object Unify {
    def unify(thes: InstantValue, that: InstantValue)(implicit r: Renaming): Result[Unit] = {
        println("UNIFY: " + thes + " " + that + " " + r)
        (thes.forced, that.forced) match {
            case (a @ Closure(_, _, _, _), b @ Closure(_, _, _, _)) =>
                (a.ty unify b.ty) orElse {
                    (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
                }
            case (Add1(a), Add1(b)) =>
                (a unify b)
            case (a @ Cons(_, _), b @ Cons(_, _)) =>
                (a.a unify b.a) orElse (a.d unify b.d)
            case (a @ Π(_, _, _, _), b @ Π(_, _, _, _)) =>
                (a.ty unify b.ty) orElse {
                    (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
                }
            case (a @ Σ(_, _, _, _), b @ Σ(_, _, _, _)) =>
                (a.ty unify b.ty) orElse {
                    (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
                }
            case (a @ Neut(_), b @ Neut(_)) =>
                (a.neutral unify b.neutral)
            case (ℕ, ℕ) | (U, U) | (Sole, Sole) | (Zero, Zero)
                | (Trivial, Trivial) | (Absurd, Absurd) => Exact(())
            case _ => ErrorInfo()
        }
    }

    def unify(thes: Neutral, that: Neutral)(implicit r: Renaming): Result[Unit] = (thes, that) match {
        case (a @ NeutVar(_, _), b @ NeutVar(_, _)) if r contains (a.name -> b.name) =>
            Exact(())
        case (a @ NeutApp(_, _), b @ NeutApp(_, _)) =>
            (a.closure unify b.closure) orElse (a.param unify b.param)
        case (a @ NeutCar(_), b @ NeutCar(_)) =>
            (a.pair unify b.pair)
        case (a @ NeutCdr(_), b @ NeutCdr(_)) =>
            (a.pair unify b.pair)
        case _ => ErrorInfo()
    }
}

