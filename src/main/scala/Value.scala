import Term._
import Gamma.Gamma

package object Value {
    sealed abstract class Value
    case class Closure(abs: Abs)(implicit gamma: Gamma) extends Value
    case class Var(name: String) extends Value

    type TValue = Value
    case class Pi(x: Value, tya: TValue, b: Term) extends TValue
    type Π = Pi; val Π = Pi
}