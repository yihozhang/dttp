package Data
import Value._
package object Gamma {
    // There are three kinds of binders: a free binder represents a free
    // variable, that was bound in some larger context by λ, Π, or Σ. A
    // def binder represents a name bound by define. A claim binder
    // doesn't actually bind a name; however, it reserves the name for
    // later definition with define and records the type that will be
    // used.
    // Quoted from pie/basic.rkt
    sealed abstract class Binding(val name: String, val ty: Value)
    case class Free(override val name: String, override val ty: Value) extends Binding(name, ty) // bound for λ, Π, or Σ
    case class Def(override val name: String, override val ty: Value, val value: Value) extends Binding(name, ty)
    case class Claim(override val name: String, override val ty: Value) extends Binding(name, ty)
    class  Gamma(private val Γ: List[Binding] = Nil) {
        private def findImpl[T](name: String, fallback: T, fun: Binding => T): T = {
            for (pair <- Γ; if pair.name == name) {
                return fun(pair)
            }
            return fallback
        }
        def findType(name: String): Option[Value] = findImpl(name, None, pair => Some(pair.ty))
        def findName(name: String): Option[Value] = findImpl(name, None, _ match {
                case Def(name, ty, value) => Some(value)
                case _ => None
        })
        def find(name: String): Option[(Value, Option[Value])] = findImpl(name, None, _ match {
            case Free(name, ty) => Some(ty, None)
            case Def(name, ty, value) => Some(ty, Some(value))
            case Claim(name, ty) => Some(ty, None)
        })
        def toEnv: Env = new Env(Γ.foldLeft[List[(String, Value)]](Nil)((lst, binding) => binding match {
            case Def(name, ty, value) => (name -> value)::lst
            case Free(name, ty) => (name -> Neut(NeutVar(name, ty)))::lst
            case Claim(name, ty) => (name -> Neut(NeutVar(name, ty)))::lst 
        }))
        def has(name: String): Boolean = findImpl(name, false, _ => true)
        override def toString(): String = Γ.toString
        def ::(binding: Binding) = new Gamma(binding::Γ)
    }
    object Gamma {
        val initial = new Gamma(Nil)
    }
    // only runtime, no type information
    
    class Env(private val ρ: List[(String, Value)]) {
        private def findImpl[T](name: String, fallback: T, fun: Value => T): T = {
            for (pair <- ρ; if pair._1 == name) {
                return fun(pair._2)
            }
            return fallback
        }
        def find(name: String): Option[Value] = findImpl(name, None, Some(_))
        def has(name: String): Boolean = findImpl(name, false, _ => true)
        def ::(binding: (String, Value)) = new Env(binding :: ρ)
        override def toString(): String = ρ.toString()
    }
    object Env {
        val initial = new Env(Nil)
    }

    type Renaming = List[(String, String)]
    object Renaming {
        val initial: Renaming = Nil
    }
}