import scala.concurrent.Future 
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.SyncVar
import java.io.{InputStream, OutputStream}
import scala.sys.process.*
import net.bulbyvr.msgpack.{*, given}
import org.msgpack.value.Value
import munit.*
import net.bulbyvr.msgpack.instances.given
import cats.effect.IO
import cats.effect.unsafe.implicits._
import cats.effect.kernel.{Resource, Ref}
import fs2.io.process.{ProcessBuilder, Processes}
import cats.syntax.all.*
import concurrent.duration.*
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends CatsEffectSuite {
  val process = 
    ProcessBuilder("nvim", "-u", "NONE", "-N", "--embed").spawn[IO]
  /*
  val sessionPlus = process.flatMap { process =>

    for {
      session <- Session(process.stdout, process.stdin)
      requests <- Ref[IO].of(List[(String, List[Value])]()).toResource
      notifications <- Ref[IO].of(List[(String, List[Value])]()).toResource
      onReq = (method: String, args: List[Value], resp: ResponseHandler) =>
        requests.update(it => it :+ (method, args)) *> resp.send[Value](s"received $method(${args.toString})".asMsgpack)
      onNotif = (method: String, args: List[Value]) =>
        notifications.update(it => it :+ (method, args))
      _ <- session.requestTopic.subscribe(10).map(it => onReq(it.method, it.args, it.resp))
        .merge(session.notificationTopic.subscribe(10).map(it => onNotif(it.method, it.args))).compile.drain.background
    } yield (session, requests, notifications)
  } */
   val session = process.flatMap { process => Session(process.stdout, process.stdin) }

  test ("send requests and recieve response") {
    session.use { session =>
      for {
        v <- session.request[Map[String, String]]("vim_eval", """{"k1": "v1", "k2": "v2"}""" )
        _ <- IO(assertEquals(v, Map("k1" -> "v1", "k2" -> "v2")))
      } yield ()
    }
  }
  test ("recieve requests and send responses") {
    session.flatMap { session =>
      for {
        resreq <- IO.deferred[List[Value]].toResource
        _ <- session.requestTopic.subscribe(10).head.evalMap {
          case RequestEvent(method, args, resp) => 
            resp.send[Value](s"Recieved $method(${args.toString})".asMsgpack) 
              *> IO.println(s"got $method(${args.toString})") 
              *> resreq.complete(args)
        }.compile.drain.background 
        v <- session.notify("vim_eval", """rpcrequest(1, "request", 1, 2, 3)""").toResource
        _ <- IO.println("hooking").toResource
        req <- resreq.get.toResource
        _ <- IO.println(req).toResource
        _ <- IO(assert(req.map(_.as[Int].get) == List(1, 2, 3))).toResource
      } yield ()
    }
  }
}
