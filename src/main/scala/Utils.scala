import Gamma._
import Src._

package object Utils {
    def fresh[T<:Src](implicit gamma: Gamma[T]): String = {
        def generate = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString
        var name = generate
        while ((gamma find name).isDefined) {
            name = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString 
        }
        name
    }
}