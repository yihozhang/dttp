import Gamma.Gamma
import Value._

package object Term {
    // type Result[T] = Either[T, Unit]
    // implicit def toResult[T](result: T): Result[T] = Left(result)

    // sealed abstract class Term {
    //     // value information
    //     def eval(implicit gamma: Gamma): Result[Value]
    // }

    // case class Var(name: String) extends Term {
    //     override def eval(implicit gamma: Gamma) = gamma find name match {
    //         case Some(term) => term.eval
    //         case None => Value.Var(name)
    //     }
    // }
    // case class App(fun: Term, ty: TTerm, param: Term) extends Term {
    //     override def eval(implicit gamma: Gamma): Result[Value] = fun match {
    //         case Abs(name, body) => body eval ((name, param)::gamma)
    //         case _ => Right ()
    //     }
    // }
    // case class Abs(name: String, body: Term) extends Term {
    //     override def eval(implicit gamma: Gamma): Result[Value] = Value.Closure(this)
    // }


    // type TTerm = Term
    // case class Fun(in: Term, out: Term) extends TTerm {
    //     override def eval(implicit gamma: Gamma): Result[Value] = in eval match {
    //         case Left(in_o) =>  Value.Π(fresh, in_o, out)
    //         case Right(info) => Right(info)
    //     } 
    // }

    // def fresh(implicit gamma: Gamma): Value = {
    //     def generate = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString
    //     var name = generate
    //     while ((gamma find name).isDefined) {
    //         name = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString 
    //     }
    //     Value.Var(name)
    // }
    // case class Num(x: Int) extends Term {
    //     override def eval(gamma: Gamma): Either[Value, Unit] = Left(Num(x))
    // }
    sealed abstract class Src
    sealed abstract class Type extends Src
    sealed abstract class Term extends Src
    trait Core extends Src
    trait Value extends Core // only core things can be values
    
    // below are terms

    // variable
    case class Var(name: String) extends Term with Value
    // applcation
    case class App(fun: Term, param: Term) extends Term with Core
    object App {
        def apply(fun: Term, param:Term, params: Term*): App = {
            params.foldLeft(App(fun, param))((fun, param) => App(fun, param))
        }
    }
    // abstraction (function)
    case class Abs(name: String, tp: Type, body: Src) extends Term with Value
    // closure
    case class Closure(name: String, out: Term, gamma: Gamma) extends Term with Value

    // below are types
    case class →(in: Type, out: Type) extends Type with Value // this is not a core
    type Arrow = →; val Arrow = →
    case class Π(name:String, in: Type, out: Type) extends Type with Value

}