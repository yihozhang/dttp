import Core.Core
import Gamma.{Gamma, Free, Def, Renaming}

package object Value {
    sealed abstract class Value {
        def forced: InstantValue
    }
    object Value {
        implicit def force(value: Value): InstantValue = value.forced
    }
    case class DelayedValue(core: Core, gamma: Gamma) extends Value {
        var value: Option[InstantValue] = None
        override def forced: InstantValue = value match {
            case None => {
                val actual: InstantValue = core.toValueImpl(gamma)
                value = Some(actual)
                actual
            }
            case Some(value) => value
        }   
    }
    sealed abstract class InstantValue extends Value {
        override val forced = this
        def describes(term: Value): Option[ErrorInfo] = {
            val termo = term.forced
            if (this.isInstanceOf[TermConstr]) {
                Some(new ErrorInfo)
            } else term synth match {
                case Left(info) => Some(info)
                case Right(termType) => this unify termType
            }
        }
        def synth: Either[ErrorInfo, Value]
        def unify(that: Value, r: Renaming = Renaming.initial): Option[ErrorInfo] = ???
    }
    trait TermConstr extends InstantValue
    trait TypeConstr extends InstantValue

    case class Closure(name: String, ty: Value, body: Core, gamma: Gamma) extends InstantValue with TermConstr {
        override def synth: Either[ErrorInfo,Value] = for {
            tyty <- ty.synth
            _ <- (tyty.forced unify U) toLeft Right()
            result <- body.toValue(Free(name, tyty)::gamma) synth
        } yield result
    }
    case object U extends InstantValue {
        override def synth: Either[ErrorInfo,Value] = Left(new ErrorInfo) // Universe has no type
    }
    case object Sole extends InstantValue with TermConstr {
        override val synth: Either[ErrorInfo, Value] = Right(Trivial)
    }
    case object Zero extends InstantValue with TermConstr {
        override val synth: Either[ErrorInfo, Value] = Right(Nat)
    }
    case class Add1(inner: Value) extends InstantValue with TermConstr {
        override def synth: Either[ErrorInfo, Value] = {
            (Nat describes inner).map(Left(_)).getOrElse(Right(Nat))
        }
    }
    case class Cons(a: Value, d: Value) extends InstantValue with TermConstr {
        override def synth: Either[ErrorInfo, Value] = for {
            aType <- a.synth
            dType <- d.synth
            result <- (aType.forced, dType.forced) match {
                case (tya, Π(name, fr, to, gamma)) => for {
                    result <- (tya unify fr) toLeft Σ(name, tya, to, gamma)
                } yield result
                case _ => Left(new ErrorInfo)
            }
        } yield result
    }

    // Π (x: A) -> B
    case class Π(x: String, fr: Value, to: Core, gamma: Gamma) extends InstantValue with TypeConstr {
        // override def describes(term: Value): Option[ErrorInfo] =
        //     term match {
        //         case Closure(name, ty, body, gamma1) => {
        //             (fr unify ty) orElse {
        //                 to.toValue(Free(x, fr)::gamma) describes (body.toValue(Free(name, ty)::gamma1))
        //             }
        //         }   
        //         case _ => Some(new ErrorInfo)
        //     }
        override def synth: Either[ErrorInfo,Value] = for {
            frType <- fr.synth
            _ <- (frType unify U) toLeft Right()
            to <- Right(to.toValue(Free(x, fr)::gamma).forced)
            toType <- to.synth
            _ <- (toType unify U) toLeft Right()
        } yield U
    }

    // Σ (A, D)
    type Pi = Π; val Pi = Π
    case class Σ(name: String, fr: Value, to: Core, gamma: Gamma) extends InstantValue with TypeConstr {
        // override def describes(term: Value): Option[ErrorInfo] =
        //     term match {
        //         case Cons(a, Closure(name, ty, body, gamma1)) => {
        //             fr.describes(a) orElse {
        //                 to.toValue(Free(name, fr)::gamma) describes (body.toValue(Def(name, ty, a)::gamma))
        //             }
        //         }
        //         case _ => Some(new ErrorInfo)
        //     } 
        override def synth: Either[ErrorInfo,Value] = Right(U) // todo, check type
    }
    type Sigma = Σ; val Sigma = Σ 
    case object Trivial extends InstantValue with TypeConstr {
        // override def describes(term: Value): Option[ErrorInfo] =
        //     term match {
        //         case Sole => None
        //         case _ => Some(new ErrorInfo)
        //     }
        override def synth: Either[ErrorInfo,Value] = Right(U) // todo, check type
    }
    case object Absurd extends InstantValue with TypeConstr {
        // override def describes(term: Value): Option[ErrorInfo] =
        //     Some(new ErrorInfo)
        override def synth: Either[ErrorInfo,Value] = Right(U) // todo, check type
    }
    case object ℕ extends InstantValue with TypeConstr {
        // override def describes(term: Value): Option[ErrorInfo] = term match {
        //     case Zero => None
        //     case Add1(inner) => ℕ.describes(inner)
        //     case _ => Some(new ErrorInfo)
        // }
        override def synth: Either[ErrorInfo,Value] = Right(U) // todo, check type}
    }
    val Nat = ℕ;
    case class Neut(neutral: Neut.Neutral) extends InstantValue {
        // override def describes(term: Value): Option[ErrorInfo] = {
        //     None // TODO: this
        // }
        override def synth: Either[ErrorInfo,Value] = ???
    }
    object Neut {
        sealed abstract class Neutral
        case class NeutVar(name: String, ty: Value) extends Neutral
        case class NeutApp(closure: Closure, param: Neutral) extends Neutral
        case class NeutCar(pair: Neutral) extends Neutral
        case class NeutCdr(pair: Neutral) extends Neutral
    }
}