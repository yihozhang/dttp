import Term._

package Gamma {
    case class Gamma(private val context: List[Pair[String, Term]] = Nil) {
        def find(name: String): Option[Term] = {
            for (pair <- context; if pair._1 == name) {
                return Some(pair._2)
            }
            return None
        }
        def ::(pair: Pair[String, Term]): Gamma = pair::context
    }

    object Gamma {
        implicit def toGamma(context: List[Pair[String, Term]]): Gamma = Gamma(context)
    }
}