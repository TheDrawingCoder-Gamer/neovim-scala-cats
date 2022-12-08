package net.bulbyvr.msgpack 

import org.msgpack.value.Value
import instances.given
// import com.fasterxml.jackson.annotation.JsonFormat

object PacketType {
    val Request = 0
    val Response = 1 
    val Notification =2
}

// @JsonFormat(shape = JsonFormat.Shape.ARRAY)
sealed trait Packet 

case class Request(packetType: Int, requestId: Long, method: String, args: List[Value]) extends Packet
object Request {
  def apply(requestId: Long, method: String, args: List[Value]) = new Request(PacketType.Request, requestId, method, args)
}

case class Response(packetType: Int, requestId: Long, error: Value, result: Value) extends Packet
object Response {
  def apply(requestId: Long, error: Value, result: Value) = new Response(PacketType.Response, requestId, error, result)
}

case class Notification(packetType: Int, method: String, args: List[Value]) extends Packet
object Notification{
  def apply(method: String, args: List[Value]) = new Notification(PacketType.Notification, method, args)
}

given packetCodec: Codec[Packet] with {
  def apply(v: Value): Option[Packet] = {
    if (v.isArrayValue()) {
      val av = v.asArrayValue().list()
      val kind = av.get(0).asIntegerValue().asInt()
      kind match {
        case 0 => 
          for {
            requestId <- av.get(1).as[Long]
            method <- av.get(2).as[String]
            args <- av.get(3).as[List[Value]]
          } yield Request(requestId, method, args)
        case 1 => 
          for {
            requestId <- av.get(1).as[Long]
            error <- Option(av.get(2))
            result <- Option(av.get(3))
          } yield Response(requestId, error, result)
        case 2 => 
          for {
            method <- av.get(1).as[String]
            args <- av.get(2).as[List[Value]]
          } yield Notification(method, args)
        case _ => None
      }

    } else None 
  }
  def apply(a: Packet): Value = {
    a match
      case Notification(packetType, method, args) => List(packetType.asMsgpack, method.asMsgpack, args.asMsgpack).asMsgpack
      case Request(packetType, requestId, method, args) => List(packetType.asMsgpack, requestId.asMsgpack, method.asMsgpack, args.asMsgpack).asMsgpack
      case Response(packetType, requestId, error, result) => List(packetType.asMsgpack, requestId.asMsgpack, error, result).asMsgpack
    
  }
}

