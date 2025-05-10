package sensetive

import io.circe.{ Json, JsonObject }
import shapeless.Poly1

import scala.language.implicitConversions

package object json extends LogShowJson

trait LogShowJson {
  final case class JsonString(value: String) extends Serializable
  final case class JsonBoolean(value: Boolean) extends Serializable
  final case class JsonArray(values: Vector[Json]) extends Serializable
  object JsonNull extends Serializable
  final case class JsonChar(value: Char) extends Serializable
  final case class JsonLong(value: Long) extends Serializable
  final case class JsonDouble(value: Double) extends Serializable

  object AsJson extends Poly1 {
    implicit def jsonObject = at[JsonObject] { jsonObject => Json.fromJsonObject(jsonObject) }
    implicit def jsonString = at[JsonString] { jsonString => Json.fromString(jsonString.value) }
    implicit def jsonBoolean = at[JsonBoolean] { jsonBoolean =>
      Json.fromBoolean(jsonBoolean.value)
    }
    implicit def jsonArray = at[JsonArray] { jsonArray => Json.fromValues(jsonArray.values) }
    implicit def jsonNull = at[JsonNull.type] { _ => Json.Null }
    implicit def jsonChar = at[JsonChar] { jsonChar => Json.fromInt(jsonChar.value) }
    implicit def jsonLong = at[JsonLong] { jsonLong => Json.fromLong(jsonLong.value) }
    implicit def jsonDouble = at[JsonDouble] { jsonDouble =>
      Json.fromDouble(jsonDouble.value).getOrElse(throwExp(s"${jsonDouble.value} is not real."))
    }
  }
  //helpers
  private def throwExp(msg: String): Nothing = throw new IllegalArgumentException(msg)
}
