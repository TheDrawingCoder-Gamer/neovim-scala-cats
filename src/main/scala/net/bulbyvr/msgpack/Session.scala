package net.bulbyvr.msgpack 

import java.io.{InputStream, OutputStream}


import scala.collection.mutable as mut
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import cats.effect.{IO, Resource}
import org.msgpack.value.Value
import io.reactivex.rxjava3.subjects.ReplaySubject 
import instances.given
import org.msgpack.value.ValueFactory
import scala.concurrent.ExecutionContext
import cats.implicits.*
import cats.effect.kernel.Deferred
import cats.effect.kernel.DeferredSource
import fs2.concurrent.Topic
import concurrent.duration.*
import fs2.{Stream => FStream}
import java.util.UUID
case class ResponseHandler(writer: (Value) => IO[Unit], requestId: Long) {
  def send[T](using Encoder[T])(resp: T*): IO[Unit] = send(resp.toList)
  def send[T](using Encoder[T])(resp: List[T]): IO[Unit] = {
    writer(Response(requestId, null, resp.asMsgpack).asMsgpack(using packetCodec))
  }
}
trait ExtendedCodec[A] extends Codec[A] {
  override def apply(v: Value): Option[A] = {
    if (v.isExtensionValue()) {
      val ev = v.asExtensionValue()
      if (ev.getType() == extendedType) {
        extendedDecode(ev.getData())
      } else None 
    } else None 
  }
  override def apply(a: A): Value = {
    val data = extendedEncode(a)
    ValueFactory.newExtension(extendedType, data)
  }
  def extendedType: Byte
  def extendedDecode(v: Array[Byte]): Option[A]
  def extendedEncode(a: A): Array[Byte]

}
sealed trait Instance[F[_]] {
  type A
  def _1: A
  def _2: F[A]
}

object Instance {
  type Aux[F[_], A0] = Instance[F] { type A = A0 }
  def apply[F[_], A0](a: A0)(using ev: F[A0]): Aux[F, A0] =
    new Instance[F] {
      override final type A = A0
      override final def _1 = a
      override final def _2 = ev
    }

  given makeInstance[F[_], A0](using ev: F[A0]): Conversion[A0, Instance[F]] =
    apply(_)(using ev)
}
case class RequestEvent(method: String, args: List[Value], resp: ResponseHandler)
case class NotificationEvent(method: String, args: List[Value])
class Session private (in: fs2.Stream[IO, Byte], out: fs2.Pipe[IO, Byte, Nothing], 
  val requestTopic: Topic[IO, RequestEvent], val notificationTopic: Topic[IO, NotificationEvent]) {

  val inPacket = Msgpack.inPacketStream(in)
  val outPacket = Msgpack.outPacketPipe(out)

  private var nextRequestId: Long = 1
  private var pendingRequests = mut.HashMap.empty[Long, Deferred[IO, Value]]
  // Create a thread to listen for any packets
  lazy val runner: fs2.Stream[IO, Nothing] = inPacket.evalMap(parseMessage).drain

  def request[T](using Decoder[T])(method: String, args: Instance[Encoder]*): IO[T] = request[T](method, args.toList)
  private def request[T](using Decoder[T])(method: String, args: List[Instance[Encoder]] = List()): IO[T] = {
    for {

      p <- IO.deferred[Value]
      // sinful
      id <- IO(nextRequestId)
      _ <- IO(nextRequestId += 1)
      _ <- IO(pendingRequests(id) = p)
      _ <- write[Packet](Request(id, method, args.map(it => it._2.apply(it._1))))
      res <- p.get
    } yield res.as[T].get
  }
  
  def notify(method: String, args: Instance[Encoder]*): IO[Unit] = notify(method, args.toList)
  private def notify(method: String, args: List[Instance[Encoder]]): IO[Unit] = write(using packetCodec)(Notification(method, args.map(it => it._2.apply(it._1))))

  private def write[T](using enc: Encoder[T])(obj: T): IO[Unit] = {
    outPacket(fs2.Stream.eval(IO(enc(obj)))).compile.drain
  }

  private def parseMessage(packet: Packet): IO[Unit] = {
    packet match {
      case Request(_, id, method, args) =>
        for {
          _ <- IO.println("received request")
          _ <- requestTopic.publish1(RequestEvent(method, args, ResponseHandler(this.write(using identityEncoder), id)))
        } yield () 

      case Response(_, id, err, result) =>
        for {
          req <- IO(pendingRequests(id))
          _ <- if (!err.isNilValue()) IO.raiseError(new java.lang.IllegalArgumentException(err.toString)) else req.complete(result)
          _ <- IO(this.pendingRequests.remove(id))
        } yield ()

      case Notification(_, method, args) =>
        for {
          _ <- notificationTopic.publish1(NotificationEvent(method, args))
        } yield ()
    }
    
  }
  def run: IO[Unit] = runner.compile.drain
}

object Session {
  def apply(stdout: fs2.Stream[IO, Byte], stdin: fs2.Pipe[IO, Byte, Nothing]): Resource[IO, Session] = {
    for {
        requestTopic <- Topic[IO, RequestEvent].toResource
        notificationTopic <- Topic[IO, NotificationEvent].toResource
        session <- IO(new Session(stdout, stdin, requestTopic, notificationTopic)).toResource
        _ <- session.run.background
    } yield session
  }
}
