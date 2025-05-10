package sensetive

import org.scalatest.{FunSuite, MustMatchers}
import sensetive.syntax._

class MaskedSpec extends FunSuite with MustMatchers {

  test("Expected 'Masked' wrapper works the same for all the types") {
    val expectedValue = s"\"${Masked.maskedString}\""
    final case class TelegramCypherKeys(key: String)
    Masked(1).sanitizeNoSpaces mustBe expectedValue
    Masked(false).sanitizeNoSpaces mustBe expectedValue
    Masked(TelegramCypherKeys("qwerty")).sanitizeNoSpaces mustBe expectedValue
  }
}