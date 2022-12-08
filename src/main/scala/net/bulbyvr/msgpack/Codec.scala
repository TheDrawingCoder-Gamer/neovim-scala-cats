package net.bulbyvr.msgpack

import org.msgpack.value.Value
import org.msgpack.value.ValueFactory
import scala.jdk.CollectionConverters.*
import cats.implicits.*
import cats.syntax.*
import scala.reflect.ClassTag
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
        arrayCodec[Byte].apply(v)
      } else if(v.isBinaryValue()) {
        Some(v.asBinaryValue().asByteArray())
      } else None
    }
    def apply(a: Array[Byte]): Value = ValueFactory.newBinary(a)
  }
  given arrayCodec[A: ClassTag]: Codec[Array[A]] with {
    def apply(v: Value)(using decoder: Decoder[A]): Option[Array[A]] = {
      if (v.isArrayValue()) {
        val av = v.asArrayValue()
        val values = av.list().asScala.toList
        for {
          v <- values.map(decoder.apply).sequence
        } yield v.toArray
      } else None 
    }
    def apply(a: Array[A])(using encoder: Encoder[A]): Value = {
      ValueFactory.newArray(a.map(encoder.apply), false)
    }
  }
  given listCodec[A]: Codec[List[A]] with {
    def apply(v: Value)(using decoder: Decoder[A]): Option[List[A]] = {
      if (v.isArrayValue()) {
        val av = v.asArrayValue()
        val values = av.list().asScala.toList
        for {
          v <- values.map(decoder.apply).sequence
        } yield v 
      } else None 
    }
    def apply(a: List[A])(using encoder: Encoder[A]): Value = {
      ValueFactory.newArray(a.map(encoder.apply).toArray, false)
    }
  }
  given seqCodec[A: ClassTag]: Codec[Seq[A]] with {
    def apply(v: Value)(using decoder: Decoder[A]): Option[Seq[A]] = arrayCodec.apply(v).map(_.toSeq)
    def apply(a: Seq[A])(using encoder: Encoder[A]): Value = arrayCodec.apply(a.toArray)
  }
  given stringCodec: Codec[String] with {
    def apply(v: Value): Option[String] = 
      if (v.isStringValue()) {
        Some(v.asStringValue().asString())
      } else None
    def apply(a: String): Value = ValueFactory.newString(a)
  }
  given mapCodec[K, V]: Codec[Map[K,V]] with {
    def apply(v: Value)(using kd: Decoder[K])(using vd: Decoder[V]): Option[Map[K, V]] = {
      if (v.isMapValue()) {
        val mv = v.asMapValue()
        for { 
          v <- mv.map().asScala.toMap.map { case (k, v) => (kd.apply(k), vd.apply(v)).tupled}.toList.sequence
        } yield v.toMap
      } else None
    }
    def apply(a: Map[K, V])(using ke: Encoder[K])(using ve: Encoder[V]): Value = {
      ValueFactory.newMap(a.map{ case (k, v) => (ke.apply(k), ve.apply(v))}.asJava)
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
}
