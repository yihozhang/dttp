
package Utils
import Data._
package object ConversionUtils {
    implicit def force(value: Value): Value.InstantValue = value.forced
    class OptionConverter(option: Option[ErrorInfo]) {
        def convert: Either[ErrorInfo, Unit] = option match {
            case None => Right(Unit)
            case Some(value) => Left(value)
        }
    }
    implicit def convert(option: Option[ErrorInfo]) = new OptionConverter(option)
}