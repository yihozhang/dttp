import Ctx._
import Src._

package object Utils {
    def fresh[T<:Src](implicit ctx: Ctx): String = {
        def generate = (1 until 10).map(_ => (math.random() * 26).toChar + 'a').map(_.toChar).mkString
        var name = generate
        while ((ctx.naming find name).isDefined || (ctx.typing find name).isDefined) {
            name = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString 
        }
        name
    }
}