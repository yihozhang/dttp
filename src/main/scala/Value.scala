import Core.Core
import Gamma.{Gamma, Free, Def, Renaming}
import Utils.someToLeftNoneToRight

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
        def describes(term: InstantValue): Option[ErrorInfo] = {
            if (this.isInstanceOf[TermConstr]) {
                Some(new ErrorInfo)
            } else term synth match {
                case Left(info) => Some(info)
                case Right(termType) => (this unify termType)
            }
        }
        def synth: Either[ErrorInfo, Value]
        def unify(that: InstantValue)(implicit r: Renaming = Renaming.initial): Option[ErrorInfo] = (this.forced, that.forced) match {
            case (a @ Closure(_, _, _, _), b @ Closure(_, _, _, _)) =>
                (a.ty unify b.ty) orElse (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
            case (Add1(a), Add1(b)) =>
                (a unify b)
            case (a @ Cons(_, _), b @ Cons(_, _)) =>
                (a.a unify b.a) orElse (a.d unify b.d)
            case (a @ Π(_, _, _, _), b @ Π(_, _, _, _)) =>
                (a.ty unify b.ty) orElse (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
            case (a @ Σ(_, _, _, _), b @ Σ(_, _, _, _)) =>
                (a.ty unify b.ty) orElse (a.selfEval unify b.selfEval)((a.name -> b.name)::r)
            case (a @ Neut(_), b @ Neut(_)) =>
                (a.neutral unify b.neutral)
            case (ℕ, ℕ) | (U, U) | (Sole, Sole) | (Zero, Zero)
                | (Trivial, Trivial) | (Absurd, Absurd) => None
            case _ => Some(new ErrorInfo)
        }
    }
    trait TermConstr extends InstantValue
    trait TypeConstr extends InstantValue

    trait ClosureLike extends InstantValue {
        val name: String
        val ty: Value
        val body: Core
        val gamma: Gamma
        var typical: Option[Value] = None
        def selfEval: Value = typical match {
            case None => {
                val actual = body.toValue(Free(name, ty)::gamma)
                typical = Some(actual)
                actual
            }
            case Some(actual) => actual
        }
    }
    case class Closure(
        override val name: String,
        override val ty: Value,
        override val body: Core,
        override val gamma: Gamma
    ) extends InstantValue with TermConstr with ClosureLike {
        
        override def synth: Either[ErrorInfo,Value] = for {
            _ <- (U describes ty).convert
            result <- selfEval synth
        } yield result
    }
    case object U extends InstantValue {
        override def synth: Either[ErrorInfo,Value] =
            Left(new ErrorInfo( "Universe has no type"))
        
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
        // first get the type for A and for D, and D should be (name:A) -> T
        // then, the result type is sigma(name, A, D)
        override def synth: Either[ErrorInfo, Value] = for {
            aType <- a.synth
            dType <- d.synth
            result <- (aType.forced, dType.forced) match {
                case (tya, Π(name, fr, to, gamma)) =>
                    (tya unify fr) toLeft Σ(name, tya, to, gamma)
                case _ => Left(new ErrorInfo())
            }
        } yield result
    }

    // Π (x: A) -> B
    case class Π(
        override val name: String,
        override val ty: Value,
        override val body: Core,
        override val gamma: Gamma
    ) extends InstantValue with TypeConstr with ClosureLike {
        override def synth: Either[ErrorInfo,Value] = for {
            _ <- (U describes ty).convert
            to <- Right(body.toValue(Free(name, ty)::gamma))
            _ <- (U describes to).convert
        } yield U
    }

    // Σ (A, D)
    type Pi = Π; val Pi = Π
    case class Σ(
        override val name: String,
        override val ty: Value,
        override val body: Core,
        override val gamma: Gamma
    ) extends InstantValue with TypeConstr with ClosureLike {
        override def synth: Either[ErrorInfo,Value] = for {
            _ <- (U describes ty).convert
            to <- Right(body.toValue(Free(name, ty)::gamma)) 
            _ <- (U describes to).convert
        } yield U
    }
    type Sigma = Σ; val Sigma = Σ 
    case object Trivial extends InstantValue with TypeConstr {
        override def synth: Either[ErrorInfo,Value] = Right(U)
    }
    case object Absurd extends InstantValue with TypeConstr {
        override def synth: Either[ErrorInfo,Value] = Right(U)
    }
    case object ℕ extends InstantValue with TypeConstr {
        override def synth: Either[ErrorInfo,Value] = Right(U)
    }
    val Nat = ℕ;
    case class Neut(neutral: Neut.Neutral) extends InstantValue {
        // override def describes(term: Value): Option[ErrorInfo] = {
        override def synth: Either[ErrorInfo,Value] = neutral synth
    }
    object Neut {
        sealed abstract class Neutral {
            def synth: Either[ErrorInfo,Value]
            def unify(that: Neutral)(implicit r: Renaming): Option[ErrorInfo] = (this, that) match {
                case (a @ NeutVar(_, _), b @ NeutVar(_, _)) if r contains (a.name -> b.name) =>
                    None
                case (a @ NeutApp(_, _), b @ NeutApp(_, _)) =>
                    (a.closure unify b.closure) orElse (a.param unify b.param)
                case (a @ NeutCar(_), b @ NeutCar(_)) =>
                    (a.pair unify b.pair)
                case (a @ NeutCdr(_), b @ NeutCdr(_)) =>
                    (a.pair unify b.pair)
                case _ => Some(new ErrorInfo)
            }
        }
        case class NeutVar(name: String, ty: Value) extends Neutral {
            def synth = Right(ty)
        }
        // Since we are doing call-by-need here, what we're eliminating is closure, not param
        case class NeutApp(closure: Neutral, param: Value) extends Neutral {
            def synth = for {
                closureType <- closure.synth
                result <- closureType match {
                    case pi @ Π(name, ty, body, gamma) => 
                        for {
                            _ <- (ty describes param).convert
                            result <- pi.selfEval.synth
                        } yield result
                    case _ => Left(new ErrorInfo)
                }
            } yield result
        }
        case class NeutCar(pair: Neutral) extends Neutral {
            def synth = for {
                pairType <- pair.synth
                result <- pairType match {
                    case sigma @ Σ(name, ty, body, gamma) => Right(ty)
                    case _ => Left(new ErrorInfo)
                }
            } yield result
        }
        case class NeutCdr(pair: Neutral) extends Neutral {
            def synth = for {
                pairType <- pair.synth
                result <- pairType match {
                    case sigma @ Σ(name, ty, body, gamma) => sigma.selfEval.synth
                    case _ => Left(new ErrorInfo)
                }
            } yield result
        }
    }
}