import Src.Src

package object Gamma {
    class Gamma[T<:Src](private val context: List[Pair[String, T]]) {
        def find(name: String): Option[T] = {
            for (pair <- context; if pair._1 == name) {
                return Some(pair._2)
            }
            return None
        }
        def ::(pair: Pair[String, T]): Gamma[T] = Gamma(pair::context)
        override def toString(): String = context.toString()
    }

    object Gamma {
        def apply[T<:Src](context: List[Pair[String, T]] = Nil): Gamma[T] = new Gamma(context)
        def empty[T<:Src]: Gamma[T] = Gamma()
    }
}