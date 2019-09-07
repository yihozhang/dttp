import Src.Core

package object Ctx {
    type Binding = Pair[String, Core]
    class  Gamma(private val Γ: List[Binding] = Nil) {
        def find(name: String): Option[Core] = {
            for (pair <- Γ; if pair._1 == name) {
                return Some(pair._2)
            }
            return None
        }
        override def toString(): String = Γ.toString
        def ::(binding: Binding) = new Gamma(binding::Γ)
    }
    class Ctx(val naming: Gamma = new Gamma, val typing: Gamma = new Gamma) {
        override def toString(): String = (naming, typing).toString()
        def andNaming(binding: Binding) = new Ctx(binding :: naming, typing)
        def andTyping(binding: Binding) = new Ctx(naming, binding :: typing)
    }
}