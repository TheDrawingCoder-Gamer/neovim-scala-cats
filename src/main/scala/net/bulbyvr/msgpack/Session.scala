package net.bulbyvr.msgpack 

import java.io.{InputStream, OutputStream}


import scala.collection.mutable as mut
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import cats.effect.IO
import org.msgpack.value.Value
import io.reactivex.rxjava3.subjects.ReplaySubject 
import instances.given
import org.msgpack.value.ValueFactory
import scala.concurrent.ExecutionContext
import com.typesafe.scalalogging.Logger
case class ResponseHandler(writer: (Value) => Unit, requestId: Long) {
  def send[T](using Encoder[T])(resp: T*): Unit = send(resp.toList)
  def send[T](using Encoder[T])(resp: List[T]): Unit = {
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
class Session(in: InputStream, out: OutputStream) {

  val logger = Logger("msgpack-session")
  private var nextRequestId: Long = 1
  private var pendingRequests = mut.HashMap.empty[Long, Promise[Value]]

  private case class RequestEvent(method: String, args: List[Value], resp: ResponseHandler)
  private val requestEvent = ReplaySubject.create[RequestEvent]()

  private case class NotificationEvent(method: String, args: List[Value])
  private val notificationEvent = ReplaySubject.create[NotificationEvent]()

  // Create a thread to listen for any packets
  new Thread(new Runnable {
    override def run(): Unit = {
      try {
        while (true) {
          val packet = Msgpack.readPacket(in)
          parseMessage(packet)
        }
      } catch {
        // end of input
        case e: NotAPacketException => logger.info("last packet recieved, exiting")
      }
    }
  }).start()

  def onRequest(callback: (String, List[Value], ResponseHandler) => Unit) =
    requestEvent.subscribe( next => callback(next.method, next.args, next.resp) )

  def onNotification(callback: (String, List[Value]) => Unit) =
    notificationEvent.subscribe( next => callback(next.method, next.args) )

  def request[T](using Decoder[T])(using ExecutionContext)(method: String, args: Instance[Encoder]*): Future[T] = request[T](method, args.toList)
  private def request[T](using Decoder[T])(using ExecutionContext)(method: String, args: List[Instance[Encoder]] = List()): Future[T] = {


    val p = Promise[Value]

    synchronized {
      val id: Long = this.nextRequestId
      this.nextRequestId += 1

      this.pendingRequests += (id -> p)
      logger.debug("writing request")
      write(using packetCodec)(Request(id, method, args.map(it => it._2.apply(it._1))))
    }

    p.future.flatMap(it => it.as[T].map(Future.successful _).getOrElse(Future.failed(new java.lang.IllegalArgumentException("Invalid value type"))) )
  }
  def notify(method: String, args: Instance[Encoder]*): Unit = notify(method, args.toList)
  private def notify(method: String, args: List[Instance[Encoder]]): Unit = write(using packetCodec)(Notification(method, args.map(it => it._2.apply(it._1))))

  private def write[T](using Encoder[T])(obj: T): Unit = {
    Msgpack.write(obj, out)
  }

  private def parseMessage(packet: Packet) = {
    logger.debug("Parsing message")
    packet match {
      case Request(_, id, method, args) =>
        this.requestEvent.onNext(RequestEvent(method, args, ResponseHandler(this.write(using identityEncoder), id)))

      case Response(_, id, err, result) =>
        logger.debug("Got response!")
        synchronized {
          this.pendingRequests(id) match {
            case handler =>
              if (err != null) handler.failure(new IllegalArgumentException(err.toString))
              handler.success(result)
          }
          this.pendingRequests.remove(id)
        }

      case Notification(_, method, args) =>
        this.notificationEvent.onNext(NotificationEvent(method, args))
      }
  }
  logger.info("Session initialized!")
}
