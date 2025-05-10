package sensetive

import sensetive.json.{
  JsonArray,
  JsonBoolean,
  JsonChar,
  JsonDouble,
  JsonLong,
  JsonNull,
  JsonString
}
import io.circe.{ Json, JsonObject }
import shapeless._
import shapeless.labelled.FieldType

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.util.control.NoStackTrace

trait LogShow[-T] { self =>
  def sanitize(t: T): SanitizeRes
  @inline final def contramap[R](f: R => T): LogShow[R] = r => self.sanitize(f(r))
}

object LogShow extends LogShowBase

trait LogShowBase
  extends LogShowBaseImplicits
    with LogShowAnyVal
    with LogShowShapelessImplicits
    with LogShowGenericImplicits {

  def apply[A](implicit logShow: LogShow[A]): LogShow[A] = logShow

  def derive[A, H](implicit
                   generic: shapeless.LabelledGeneric.Aux[A, H],
                   hEncoder: Lazy[LogShow[H]]
                  ): LogShow[A] = { logShowProduct }

  object NoOp extends LogShow[Any] {
    override def sanitize(t: Any): SanitizeRes = Coproduct[SanitizeRes](JsonNull)
  }
}

trait LogShowBaseImplicits {
  final def logShow[A](f: A => SanitizeRes): LogShow[A] = f(_)

  //scalar classes
  implicit val stringLogShow: LogShow[String] = logShow(s => Coproduct[SanitizeRes](JsonString(s)))
  implicit val booleanLogShow: LogShow[Boolean] =
    logShow(b => Coproduct[SanitizeRes](JsonBoolean(b)))
  implicit val longLogShow: LogShow[Long] = logShow(l => Coproduct[SanitizeRes](JsonLong(l)))
  implicit val doubleLogShow: LogShow[Double] = logShow(d => Coproduct[SanitizeRes](JsonDouble(d)))
  implicit val charLoShow: LogShow[Char] = logShow(c => Coproduct[SanitizeRes](JsonChar(c)))
  //
  implicit val byteLogShow: LogShow[Byte] = longLogShow.contramap(_.toLong)
  implicit val shortLogShow: LogShow[Short] = longLogShow.contramap(_.toLong)
  implicit val intLogShow: LogShow[Int] = longLogShow.contramap(_.toLong)
  implicit val floatLogShow: LogShow[Float] = doubleLogShow.contramap(_.toDouble)

  implicit def logShowForOption[A](implicit A: LogShow[A]): LogShow[Option[A]] = {
    case Some(a) => A.sanitize(a)
    case _ => Coproduct[SanitizeRes](JsonNull)
  }

  private def sanitizeIterator[A](i: Iterator[A])(implicit L: LogShow[A]): SanitizeRes = {
    val buf = new ListBuffer[Json]()
    while (i.hasNext) {
      buf += L.sanitize(i.next()).fold(json.AsJson)
    }
    Coproduct[SanitizeRes](JsonArray(buf.toVector))
  }

  implicit def logShowForTuple2[A, B](implicit
                                      aShow: LogShow[A],
                                      bShow: LogShow[B]
                                     ): LogShow[(A, B)] = (t: (A, B)) =>
    Coproduct[SanitizeRes](
      JsonObject(
        "_1" -> aShow.sanitize(t._1).fold(json.AsJson),
        "_2" -> bShow.sanitize(t._2).fold(json.AsJson)
      )
    )

  implicit def logShowForTuple3[A, B, C](implicit
                                         aShow: LogShow[A],
                                         bShow: LogShow[B],
                                         cShow: LogShow[C]
                                        ): LogShow[(A, B, C)] = (t: (A, B, C)) =>
    Coproduct[SanitizeRes](
      JsonObject(
        "_1" -> aShow.sanitize(t._1).fold(json.AsJson),
        "_2" -> bShow.sanitize(t._2).fold(json.AsJson),
        "_3" -> cShow.sanitize(t._3).fold(json.AsJson)
      )
    )

  implicit def iterableLogShow[A](implicit
                                  L: LogShow[A]
                                 ): LogShow[Iterable[A]] = (t: Iterable[A]) => sanitizeIterator(t.iterator)

  implicit def logShowForArray[A: LogShow]: LogShow[Array[A]] = (t: Array[A]) =>
    sanitizeIterator(t.iterator)
}

trait IsValueClass[Out] {
  type In
  def unwrap(a: Out): In
}

object IsValueClass {
  type Aux[O, I] = IsValueClass[O] { type In = I }
  def apply[A](implicit isValueClass: IsValueClass[A]): IsValueClass[A] = isValueClass
  implicit def derive[A, B]: IsValueClass.Aux[A, B] = macro AnyValHelper.isValueClass[A, B]
}

import scala.reflect.macros.whitebox

class AnyValHelper(val c: whitebox.Context) {

  import c.universe._

  private val anyVal = c.typeOf[AnyVal].typeSymbol

  def isValueClass[O: c.WeakTypeTag, I: c.WeakTypeTag]: Tree = {

    val typ = c.weakTypeOf[O]
    val sym = typ.typeSymbol

    if (sym.isClass && sym.asClass.isCaseClass && typ.baseClasses.contains(anyVal)) {

      val wrapped = c.weakTypeOf[O].decls.filter(_.isTerm).head
      c.Expr[IsValueClass[O]](q""" {
       new IsValueClass[$typ] {
          override type In = ${wrapped.asTerm.typeSignature}
          def unwrap(a: $typ): ${wrapped.asTerm.typeSignature} = a.${wrapped.asTerm.name}
       }
     }
     """).tree

    } else throw new Exception("Not a case class") with NoStackTrace
  }
}

trait LogShowAnyVal {
  implicit def valueClass[A <: AnyVal, B](implicit
                                          I: IsValueClass.Aux[A, B],
                                          ls: LogShow[B]
                                         ): LogShow[A] = (t: A) => ls.sanitize(I.unwrap(t))
}

trait LogShowShapelessImplicits {

  implicit val logShowHNil: LogShow[HNil] = (_: HNil) => Coproduct[SanitizeRes](JsonNull)

  implicit val logShowCNil: LogShow[CNil] = (_: CNil) => Coproduct[SanitizeRes](JsonNull)

  implicit def logShowHList[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hLogShow: Lazy[LogShow[H]],
                                                        tLogShow: Lazy[LogShow[T]]
                                                       ): LogShow[FieldType[K, H] :: T] = (hlist: FieldType[K, H] :: T) => {
    val head = hlist.head
    val tail = hlist.tail
    val fieldName = witness.value.name
    val jsonValue = hLogShow.value.sanitize(head)
    val unwrappedJsonObject = jsonValue.fold(json.AsJson)
    val restFields = tLogShow.value.sanitize(tail)
    Coproduct[SanitizeRes](
      restFields
        .select[JsonObject].map(jsonObject =>
          jsonObject.+:(fieldName -> unwrappedJsonObject)
        ).getOrElse(JsonObject(fieldName -> unwrappedJsonObject))
    )
  }

  implicit def logShowCoproduct[K <: Symbol, H, T <: Coproduct](implicit
                                                                hEncoder: LogShow[H],
                                                                tEncoder: Lazy[LogShow[T]]
                                                               ): LogShow[FieldType[K, H] :+: T] = {
    case Inl(h) =>
      hEncoder.sanitize(h)
    case Inr(t) =>
      tEncoder.value.sanitize(t)
  }
}

trait LogShowGenericImplicits {

  //main entry for every class
  implicit def logShowProduct[A, H](implicit
                                    ev: A <:!< AnyVal,
                                    generic: shapeless.LabelledGeneric.Aux[A, H],
                                    hEncoder: Lazy[LogShow[H]]
                                   ): LogShow[A] = (t: A) => {
    hEncoder.value.sanitize(generic.to(t))
  }
}




