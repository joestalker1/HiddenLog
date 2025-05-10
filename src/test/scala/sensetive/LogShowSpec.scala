package sensetive

import org.scalatest.{FunSuite, Matchers}
import sensetive.{LogShow, Masked, json}

import scala.language.implicitConversions
import sensetive.syntax._

class LogShowSpec extends FunSuite with Matchers {

  test("LogShow summon some basic AnyVal types") {
    1.sanitizeNoSpaces shouldBe "1"
    true.sanitizeNoSpaces shouldBe "true"
    4.4366f.sanitizeNoSpaces.startsWith("4.43") shouldBe true
  }

  test("LogShow derive log formatter for flat product types") {
    ProductType(
      42,
      "This is the episode six..."
    ).sanitizeNoSpaces shouldBe "{\"i\":42,\"s\":\"This is the episode six...\"}"
  }

  test("LogShow derive log formatter for coproduct") {
    MeetingEvent(
      "id1",
      Credentials("id1", Option("user1"), "password1")
    ).sanitizeNoSpaces shouldBe "{\"userId\":\"id1\",\"credentials\":{\"id\":\"id1\",\"user\":\"user1\",\"password\":\"password1\"}}"
  }

  test("LogShow derive log formatter for sequences of basic type") {
    SecuredInfo(
      1,
      id2 = false,
      List("Here", "I", "am")
    ).sanitizeNoSpaces shouldBe "{\"id1\":1,\"id2\":false,\"id3\":[\"Here\",\"I\",\"am\"]}"
  }

  test("LogShow derive log formatter for Map structure") {
    MapContainer(
      "some-id",
      Map("cica" -> Cat("cica cat"), "tom" -> Cat("nice"))
    ).sanitizeNoSpaces shouldBe "{\"id\":\"some-id\",\"test\":[{\"_1\":\"cica\",\"_2\":{\"name\":\"cica cat\"}},{\"_1\":\"tom\",\"_2\":{\"name\":\"nice\"}}]}"
  }

  test("LogShow derive things recursively") {
    Wrapper(Some(Wrapped("test"))).sanitizeNoSpaces shouldBe "{\"wrapped\":\"test\"}"
    Wrapper(None).sanitizeNoSpaces shouldBe "{\"wrapped\":null}"
  }

  test("LogShow derive AnyVal") {
    LogShow[Wrapped].sanitize(Wrapped("sdasd")).fold(json.AsJson).noSpaces shouldBe "\"sdasd\""
  }

  test("LogShow must hide the field Masked[String]") {
    ObjectWithMaskedField(
      "username",
      Masked("secret_password"),
      Masked(100),
      Masked(1.0f)
    ).sanitizeNoSpaces shouldBe "{\"username\":\"username\",\"password\":\"<masked>\",\"count1\":\"<masked>\",\"count2\":\"<masked>\"}"
  }

  test("LogShow string interpolator masks the Masked class") {
    import sensetive.interpolator._
    val obj = ObjectWithMaskedField(
      "username",
      Masked("secret_password"),
      Masked(100),
      Masked(1.0f)
    )
    val a = 1
    mask"there is the sanitize $obj : $a" shouldBe "there is the sanitize {\"username\":\"username\",\"password\":\"<masked>\",\"count1\":\"<masked>\",\"count2\":\"<masked>\"} : 1"
    mask2"there is the sanitize $obj" shouldBe "there is the sanitize {\n  \"username\" : \"username\",\n  \"password\" : \"<masked>\",\n  \"count1\" : \"<masked>\",\n  \"count2\" : \"<masked>\"\n}"
  }
}

final case class ProductType(i: Int, s: String)

object ProductType {
  implicit val log: LogShow[ProductType] = LogShow.derive
}

sealed trait Animal

case class Cat(name: String) extends Animal

case class Dog(name: String, breed: String) extends Animal

case class Credentials(id: String, user: Option[String], password: String)

case class MeetingEvent(userId: String, credentials: Credentials)

case class Wrapper(wrapped: Option[Wrapped])

object Wrapper {
  implicit val logShow: LogShow[Wrapper] = LogShow.derive
}

case class Wrapped(id: String) extends AnyVal

case class CompositeMeetingEvent(id: String, events: List[MeetingEvent])

final case class SecuredInfo(id1: Int, id2: Boolean, id3: List[String])

object SecuredInfo {
  implicit val logShow: LogShow[SecuredInfo] = LogShow.derive
}

final case class MapContainer(id: String, test: Map[String, Cat])

object MapContainer {
  implicit val logShow: LogShow[MapContainer] = LogShow.derive
}

final case class ObjectWithMaskedField(
                                        username: String,
                                        password: Masked[String],
                                        count1: Masked[Int],
                                        count2: Masked[Float]
                                      )
