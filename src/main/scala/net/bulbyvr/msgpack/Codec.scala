package net.bulbyvr.msgpack

import org.msgpack.value.Value
import org.msgpack.value.ValueFactory
import scala.jdk.CollectionConverters.*
import cats.implicits.*
import cats.syntax.*
import scala.reflect.ClassTag
import io.circe.Json
import org.msgpack.value.ValueType
trait Decoder[A] {
  def apply(v: Value): Option[A]
}

trait Encoder[A] {
  def apply(a: A): Value 
}

trait Codec[A] extends Decoder[A], Encoder[A] {
  def encode(a: A): Value = apply(a)
  def decode(v: Value): Option[A] = apply(v)
}
extension[A] (c: A)(using enc: Encoder[A]) {
  def asMsgpack: Value = enc.apply(c)
}
extension (v: Value) {
  def as[A](using dec: Decoder[A]) = dec.apply(v)
}
// if it has a parameter split it into encoder and decoder to prevent sadness
// (allows passing of custom usings)
package instances {
  given intCodec: Codec[Int] with {
    def apply(v: Value): Option[Int] = {
      if (v.isIntegerValue()) {
        val intValue = v.asIntegerValue()
        if (intValue.isInIntRange()) 
          Some(intValue.asInt())
        else None 
      } else None 
    }
    def apply(a: Int): Value = {
      ValueFactory.newInteger(a)
    }
  }
  given byteCodec: Codec[Byte] with {
    def apply(v: Value): Option[Byte] = {
      if (v.isIntegerValue()) {
        val intValue = v.asIntegerValue()
        if (intValue.isInByteRange()) 
          Some(intValue.asByte())
        else None 
      } else None 
    }
    def apply(a: Byte): Value = {
      ValueFactory.newInteger(a)
    }
  }
  given longCodec: Codec[Long] with {
    def apply(v: Value): Option[Long] = {
      if (v.isIntegerValue()) {
        val intValue = v.asIntegerValue()
        if (intValue.isInLongRange()) 
          Some(intValue.asLong())
        else None 
      } else None 
    }
    def apply(a: Long): Value = {
      ValueFactory.newInteger(a)
    }
  }
  given doubleCodec: Codec[Double] with {
    def apply(v: Value): Option[Double] = {
      if (v.isNumberValue()) {
        val dv = v.asNumberValue()
        Some(dv.toDouble)
      } else None 
    }
    def apply(a: Double): Value = ValueFactory.newFloat(a)
  }
  given byteArrayCodec: Codec[Array[Byte]] with {
    def apply(v: Value): Option[Array[Byte]] = {
      if (v.isArrayValue()) {
        arrayDecoder[Byte].apply(v)
      } else if(v.isBinaryValue()) {
        Some(v.asBinaryValue().asByteArray())
      } else None
    }
    def apply(a: Array[Byte]): Value = ValueFactory.newBinary(a)
  }



  given arrayEncoder[A: ClassTag](using encoder: Encoder[A]): Encoder[Array[A]] = it => {
    ValueFactory.newArray(it.map(encoder.apply), false)
  }
  given arrayDecoder[A: ClassTag](using decoder: Decoder[A]): Decoder[Array[A]] = v => {
    if (v.isArrayValue) {
      val av = v.asArrayValue()
      val values = av.list().asScala.toList 
      for {
        v <- values.map(decoder.apply).sequence
      } yield v.toArray
    } else None 
  }
  given listDecoder[A](using decoder: Decoder[A]): Decoder[List[A]] with {
    def apply(v: Value): Option[List[A]] = {
      if (v.isArrayValue()) {
        val av = v.asArrayValue()
        val values = av.list().asScala.toList
        for {
          v <- values.map(decoder.apply).sequence
        } yield v 
      } else None 
    }
  }
  given listEncoder[A](using encoder: Encoder[A]): Encoder[List[A]] with {
    def apply(a: List[A]): Value = {
      ValueFactory.newArray(a.map(encoder.apply).toArray, false)
    }
  }
  given seqEncoder[A](using enc: Encoder[A]): Encoder[Seq[A]] = it => listEncoder.apply(it.toList)
  given seqDecoder[A](using dec: Decoder[A]): Decoder[Seq[A]] = it => listDecoder.apply(it).map(_.toSeq) 
  
  given stringCodec: Codec[String] with {
    def apply(v: Value): Option[String] = 
      if (v.isStringValue()) {
        Some(v.asStringValue().asString())
      } else None
    def apply(a: String): Value = ValueFactory.newString(a)
  }

  given mapEncoder[K, V](using ke: Encoder[K])(using ve: Encoder[V]): Encoder[Map[K,V]] with {
    def apply(a: Map[K, V]): Value = {
      ValueFactory.newMap(a.map{ case (k, v) => (ke.apply(k), ve.apply(v))}.asJava)
    }
  }
  given mapDecoder[K, V](using kd: Decoder[K])(using vd: Decoder[V]): Decoder[Map[K,V]] with {
    def apply(v: Value): Option[Map[K, V]] = {
      if (v.isMapValue()) {
        val mv = v.asMapValue()
        for { 
          v <- mv.map().asScala.toMap.map { case (k, v) => (kd.apply(k), vd.apply(v)).tupled}.toList.sequence
        } yield v.toMap
      } else None
    }
  }
  given booleanCodec: Codec[Boolean] with {
    def apply(v: Value): Option[Boolean] = {
      if (v.isBooleanValue())
        Some(v.asBooleanValue().getBoolean())
      else 
        None
    }
    def apply(a: Boolean): Value = {
      ValueFactory.newBoolean(a)
    }
  }
  given identityEncoder: Encoder[Value] with {
    def apply(a: Value): Value = a 

  }
  given identityDecoder: Decoder[Value] with {
    def apply(a: Value): Option[Value] = Some(a) 

  }
  /**
      * Prefer custom codecs over this json codec. Transforming via json requires explicit conversion to json. 
      */
  given jsonCodec: Codec[Json] with {
    def apply(v: Value): Option[Json] = {
      io.circe.parser.parse(v.toJson()).toOption
    }
    def apply(a: Json): Value = {
      a.fold(
          ValueFactory.newNil(),
          b => ValueFactory.newBoolean(b),
          n => n.toBigInt.map(it => ValueFactory.newInteger(it.bigInteger)).getOrElse(ValueFactory.newFloat(n.toDouble)),
          s => ValueFactory.newString(s),
          arr => ValueFactory.newArray(arr.map(this.apply _).toArray, false),
          // uses recursion
          obj => obj.toMap.asMsgpack 
        )
    } 
  }
}
