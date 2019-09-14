package Data

import Gamma._

package object Src {
    sealed abstract class Src {
        val loc: Src.Loc
    }
    case class Loc()
    case class λ(override val loc: Loc, name: String, ty: Src, body: Src) extends Src
    case class Var(override val loc: Loc, name: String) extends Src
    case class App(override val loc: Loc, closure: Src, param: Src) extends Src
    case class Car(override val loc: Loc, pair: Src) extends Src
    case class Cdr(override val loc: Loc, pair: Src) extends Src
    case class U(override val loc: Loc) extends Src 
    case class Sole(override val loc: Loc) extends Src 
    case class Zero(override val loc: Loc) extends Src 
    case class Add1(override val loc: Loc, inner: Src) extends Src 
    case class Cons(override val loc: Loc, a: Src, d: Src) extends Src 
    case class →(override val loc: Loc, a: Src, b: Src) extends Src
    case class Π(override val loc: Loc, name: String, ty: Src, body: Src) extends Src
    case class Pair(override val loc: Loc, a: Src, b: Src) extends Src
    case class Σ(override val loc: Loc, name: String, ty: Src, body: Src) extends Src 
    case class Trivial(override val loc: Loc) extends Src 
    case class Absurd(override val loc: Loc) extends Src 
    case class ℕ(override val loc: Loc) extends Src
    type Abs = λ; val Abs = λ
    type Arrow = →; val Arrow = →
    type Pi = Π; val Pi = Π
    type Sigma = Σ; val Sigma = Σ
    type Nat = ℕ; val Nat = ℕ
}