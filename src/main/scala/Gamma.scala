import Src.Src

package object Gamma {
    case class Gamma[T<:Src](private val context: List[Pair[String, T]] = Nil) {
        def find(name: String): Option[T] = {
            for (pair <- context; if pair._1 == name) {
                return Some(pair._2)
            }
            return None
        }
        def ::(pair: Pair[String, T]): Gamma[T] = Gamma(pair::context)
    }

    object Gamma {
        def toGamma[T<:Src](context: List[Pair[String, T]]): Gamma[T] = Gamma(context)
    }
}