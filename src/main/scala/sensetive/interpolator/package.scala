package sensetive

import scala.language.implicitConversions

package object interpolator {
  private final case class NoIndentation(override val toString: String) extends AnyVal
  private final case class TwoSpaces(override val toString: String) extends AnyVal

  private object NoIndentation {
    implicit def mat[A](x: A)(implicit logShow: LogShow[A]): NoIndentation = NoIndentation(
      logShow.sanitize(x).fold(json.AsJson).noSpaces
    )
  }

  private object TwoSpaces {
    implicit def mat[A](x: A)(implicit logShow: LogShow[A]): TwoSpaces = TwoSpaces(
      logShow.sanitize(x).fold(json.AsJson).spaces2
    )
  }

  implicit class StringContextHelper(val sc: StringContext) extends AnyVal {

    /** Sanitize objects and represent it as JSON without indentations.
     ** *@param args
     ** *@return
     */
    def mask(args: NoIndentation*): String = sc.s(args: _*)

    /**
     ** Sanitize objects and represent it as JSON with two spaces indentation.
     ** *@param args
     ** *@return
     */
    def mask2(args: TwoSpaces*): String = sc.s(args: _*)
  }
}
