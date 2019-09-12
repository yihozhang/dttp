package Data

package object Result {
    sealed abstract class Result[+T] {
        def orElse[B >: T](alt: => Result[B]): Result[B] = this match {
            case Exact(_) => alt
            case ErrorInfo(msg) => this
        }
        def flatMap[B](fun: T => Result[B]): Result[B] = this match {
            case Exact(value) => fun(value)
            case thes @ ErrorInfo(_) => thes
        }
        def map[B](fun: T => B): Result[B] = this match {
            case Exact(value) => Exact(fun(value))
            case thes @ ErrorInfo(msg) => thes
        }
        // def filter(predicate: T -> Boolean) = this match {
        //     case thes @ ErrorInfo(_) => thes
        //     case Exact(value) => if (predicate(value)) this else ErrorInfo("Assertion")
        // }
    }
    case class ErrorInfo(msg: String = "") extends Result[Nothing]
    case class Exact[T](value: T) extends Result[T]
}
