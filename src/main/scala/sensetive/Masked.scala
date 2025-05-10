package sensetive

import sensetive.json.JsonString
import io.circe.Json
import shapeless.Coproduct

case class Masked[T](value: T) extends AnyVal

object Masked {
  val maskedString: String = "<masked>"

  val maskedSanitize = Coproduct[SanitizeRes](JsonString(maskedString))

  val maskedAsJson = maskedSanitize.fold(json.AsJson)
  implicit def logShow0[T]: LogShow[Masked[T]] = (_: Masked[T]) => maskedSanitize

}