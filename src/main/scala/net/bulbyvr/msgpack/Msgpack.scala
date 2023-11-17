package net.bulbyvr.msgpack

import java.io.{InputStream, OutputStream}

import org.msgpack.core.MessagePack
import instances.given
import cats.effect.{IO, Async}
import cats.effect.implicits.*
import cats.implicits.*
import cats.effect.std.Dispatcher
import org.msgpack.value.Value
// import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
// import com.fasterxml.jackson.databind.{JsonSerializer, ObjectMapper}
// import com.fasterxml.jackson.databind.module.SimpleModule
// import com.fasterxml.jackson.module.scala.DefaultScalaModule
// import net.bulbyvr.msgpack.jackson.{CustomFactory, CustomSerializer}

class NotAPacketException extends IllegalArgumentException("Invalid Packet Type")
object Msgpack {
  def inPacketStream(in: fs2.Stream[IO, Byte]): fs2.Stream[IO, Packet] = {
    fs2.io.toInputStream[IO](using Async[IO])(in).evalMap(it => IO(MessagePack.newDefaultUnpacker(it))).flatMap { unpacker => 
      fs2.Stream.eval { IO(()).untilM_(IO(unpacker.hasNext())) *> IO(unpacker.unpackValue()) }.repeat
    }.evalMapFilter(it => IO(packetCodec(it)))
  }
  def outPacketPipe(out: fs2.Pipe[IO, Byte, Nothing]): fs2.Pipe[IO, Value, Nothing] = in => {
    out(fs2.io.readOutputStream(64) { os => 
      for {
        packer <- IO(MessagePack.newDefaultPacker(os))
        _ <- in.evalMap(it => IO(packer.packValue(it)) *> IO(packer.flush())).compile.drain
      } yield ()
    })
   // out(fs2.Stream[fs2.Pure, Byte](0x65, 0x67, 0x67).covaryAll[IO, Byte])
  }
}
