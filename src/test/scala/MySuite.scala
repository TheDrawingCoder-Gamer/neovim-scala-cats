import scala.concurrent.Future 
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.SyncVar
import java.io.{InputStream, OutputStream}
import scala.sys.process.*
import net.bulbyvr.msgpack.{*, given}
import org.msgpack.value.Value
import munit.*
import net.bulbyvr.msgpack.instances.given
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends FunSuite {
  val inputStream = new SyncVar[InputStream] 
  val outputStream = new SyncVar[OutputStream]
  var session: Session = null 
  var requests: Array[(String, List[Value])] = Array()
  var notifications: Array[(String, List[Value])] = Array()
  override def beforeAll(): Unit = { 
    try {
      val pb = Process(Seq("nvim", "-u", "NONE", "-N", "--embed"))
      val pio = new ProcessIO(
        stdout => outputStream.put(stdout),
        stdin => inputStream.put(stdin),
        _ => ())
      pb.run(pio)
    } catch {
      case e: Exception =>
        println("A Neovim installation is required to run the tests")
        println("(see https://github.com/neovim/neovim/wiki/Installing)")
        System.exit(1)
    }
    session =  Session(inputStream.get, outputStream.get)
    session.onRequest((method, args, resp) => {
      requests = requests :+ (method, args)
      resp.send(s"recieved  $method(${args.toString})".asMsgpack)
    })
    session.onNotification((method, args) => {
      notifications = notifications :+ (method, args)
    })
  }

  override def beforeEach(context: BeforeEach): Unit = {
    requests = Array()
    notifications = Array()
  }

  test ("send requests and recieve response") {
    val f: Future[Map[String, String]] = session.request[Map[String, String]]("vim_eval", """{"k1": "v1", "k2": "v2"}""" ) 
    f.map { res => 
      assertEquals(res, Map("k1" -> "v1", "k2" -> "v2"))
    } 
  }
}
