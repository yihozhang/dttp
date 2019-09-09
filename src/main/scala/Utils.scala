import Gamma._
import Core._

package object Utils {
    def fresh(implicit gamma: Gamma): String = {
        def generate = (1 until 10).map(_ => (math.random() * 26).toChar + 'a').map(_.toChar).mkString
        var name = generate
        while (gamma has name) {
            name = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString 
        }
        name
    }
    class LeftRightTrick(value: Option[ErrorInfo]) {
        def convert: Either[ErrorInfo, Unit] = value match {
            case Some(info) => Left(info)
            case None => Right() 
        }
    }
    implicit def someToLeftNoneToRight(value: Option[ErrorInfo]): LeftRightTrick
        =  new LeftRightTrick(value)
}