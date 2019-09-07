import Src._
import Gamma._
object Main {
    def main(args: Array[String]): scala.Unit = {
        val program: Core = App(Abs("x", Nat, Add1(Var("x"))), Zero)
        println(program.toValue(Gamma.empty[Core]))
    }
}

// val id = Abs("s", Var("s"))