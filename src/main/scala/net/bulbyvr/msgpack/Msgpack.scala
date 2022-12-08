package net.bulbyvr.msgpack

import java.io.{InputStream, OutputStream}

import org.msgpack.core.MessagePack
import instances.given
// import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
// import com.fasterxml.jackson.databind.{JsonSerializer, ObjectMapper}
// import com.fasterxml.jackson.databind.module.SimpleModule
// import com.fasterxml.jackson.module.scala.DefaultScalaModule
// import net.bulbyvr.msgpack.jackson.{CustomFactory, CustomSerializer}

class NotAPacketException extends IllegalArgumentException("Invalid Packet Type")
object Msgpack {
  @throws[NotAPacketException]("if input is not a packet")
  def readPacket(in: InputStream): Packet = {
    val value = MessagePack.newDefaultUnpacker(in).unpackValue()
    value.as[Packet].getOrElse(throw NotAPacketException())
  }

  def write[T](using enc: Encoder[T])(obj: T, out: OutputStream): Unit = {
    val packer = MessagePack.newDefaultPacker(out)
    packer.packValue(enc.apply(obj))
  }
}
