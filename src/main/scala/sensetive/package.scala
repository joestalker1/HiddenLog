import sensetive.json.{
  JsonArray,
  JsonBoolean,
  JsonChar,
  JsonDouble,
  JsonLong,
  JsonNull,
  JsonString
}
import io.circe._
import shapeless.{ :+:, CNil }

package object sensetive {
  type SanitizeRes =
    JsonObject :+: JsonDouble :+: JsonLong :+: JsonChar :+: JsonString :+: JsonArray :+: JsonBoolean :+: JsonNull.type :+: CNil

  object syntax extends LogShowSyntax

  trait LogShowSyntax {
    implicit class LogShowOps[A](target: A)(implicit logShow: LogShow[A]) {
      lazy val sanitizeNoSpaces: String = sanitizeJson.noSpaces
      lazy val sanitizeSpaces2: String = sanitizeJson.spaces2
      //helpers
      private val sanitizeJson: Json = logShow.sanitize(target).fold(json.AsJson)
    }
  }
}