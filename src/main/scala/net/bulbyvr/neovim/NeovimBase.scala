package net.bulbyvr.neovim

import java.io.{InputStream, OutputStream}

import net.bulbyvr.msgpack.{ResponseHandler, Session}
import org.msgpack.value.Value
import net.bulbyvr.msgpack.*
import cats.effect.IO
import cats.implicits.* 
import cats.syntax.all.*
import fs2.Stream as FStream
case class NvimRequest(method: String, args: List[NeovimType], resp: ResponseHandler)
case class NvimNotification(method: String, args: List[NeovimType])
class NeovimBase(val session: Session) {
  def onRequest(): fs2.Stream[IO, NvimRequest]  =
    val topic = session.requestTopic
    for {
      it <- FStream.eval(IO.pure(topic.imap[NvimRequest] { case RequestEvent(method, args, resp) =>
        NvimRequest(method, args.map(_.as[NeovimType].get), resp)

      } { case NvimRequest(method, args, resp) => 
        RequestEvent(method, args.map(_.asMsgpack), resp)
      }))
      data <- it.subscribe(10)
    } yield data

  def onNotification(): fs2.Stream[IO, NvimNotification] =
    val topic = session.notificationTopic
    for {
      it <- FStream.eval(IO.pure(topic.imap[NvimNotification] { case NotificationEvent(method, args) =>
        NvimNotification(method, args.map(_.as[NeovimType].get))

      } { case NvimNotification(method, args) => 
        NotificationEvent(method, args.map(_.asMsgpack))
      }))
      data <- it.subscribe(10)
    } yield data
}
