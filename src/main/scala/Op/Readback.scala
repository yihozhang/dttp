package Op
import Data.Value.Value
import Data._
import Data.Core.Core
import Data.Core._
import Data.Value._
package object Readback {
    def readback(value: Value): Core = value match {
        case value @ Value.DelayedValue(core, ρ) => readback(value.forced)
        case Value.Add1(inner) => Core.Add1(readback(inner))
        case Value.Zero => Core.Zero
        case Value.Neut(neutral) => readback(neutral)
        case Value.Π(name, ty, body, ρ) => Core.Π(name, readback(ty), readback(body.toValue((name -> Neut(NeutVar(name, ty)))::ρ)))
        case Value.Closure(name, ty, body, ρ) =>
            Core.λ(name, readback(ty), readback(body.toValue((name -> Neut(NeutVar(name, ty)))::ρ)))
        case Value.Trivial => Core.Trivial
        case Value.Cons(a, d) => Core.Cons(readback(a), readback(d))
        case Value.Absurd => Core.Absurd
        case Value.ℕ => Core.ℕ
        case Value.U => Core.U
        case Value.Σ(name, ty, body, ρ) => Core.Σ(name, readback(ty), readback(body.toValue((name -> Neut(NeutVar(name, ty)))::ρ)))
        case Value.Sole => Core.Sole
        case Value.Same(value) => Core.Same(value.readback)
        case Value.≡(ty, value) => Core.≡(ty.readback, value.readback)
    }

    def readback(neutral: Value.Neutral): Core = neutral match {
        case NeutApp(closure, param) => Core.App(readback(closure), readback(param))
        case NeutCar(pair) => Core.Cdr(readback(pair))
        case NeutCdr(pair) => Core.Car(readback(pair))
        case NeutVar(name, ty) => Core.Var(name)
    }
}