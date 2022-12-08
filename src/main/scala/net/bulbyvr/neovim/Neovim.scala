package net.bulbyvr.neovim 

import java.io.InputStream
import java.io.OutputStream
import scala.concurrent.Future
import net.bulbyvr.msgpack.*
import net.bulbyvr.msgpack.instances.given
import org.msgpack.value.Value
import org.msgpack.value.ValueFactory
import scala.concurrent.ExecutionContext
import cats.implicits.*
case class Vec2i(x: Int, y: Int)
given vec2iCodec: Codec[Vec2i] with {
  def apply(a: Value): Option[Vec2i] = {
    if (a.isArrayValue()) {
      val av = a.asArrayValue().list() 
      for {
        x <- Option(av.get(0).asIntegerValue().asInt())
        y <- Option(av.get(1).asIntegerValue().asInt())
      } yield Vec2i(x, y)
    } else None
  }
   def apply(v: Vec2i): Value = ValueFactory.newArray(v.x.asMsgpack, v.y.asMsgpack)
}
def bufferCodec(session: Session)(using ExecutionContext): ExtendedCodec[Buffer] = new ExtendedCodec[Buffer] {
  def extendedType = 0 
  def extendedDecode(a: Array[Byte]): Option[Buffer] = {
    Some(Buffer(session, a)) 
  }
  def extendedEncode(a: Buffer): Array[Byte] = {
    a.data 
  }
}
def windowCodec(session: Session)(using ExecutionContext): ExtendedCodec[Window] = new ExtendedCodec[Window] {
  def extendedType = 1
  def extendedDecode(a: Array[Byte]): Option[Window] = {
    Some(Window(session, a)) 
  }
  def extendedEncode(a: Window): Array[Byte] = {
    a.data 
  }
}
def tabpageCodec(session: Session)(using ExecutionContext): ExtendedCodec[Tabpage] = new ExtendedCodec[Tabpage] {
  def extendedType = 0 
  def extendedDecode(a: Array[Byte]): Option[Tabpage] = {
    Some(Tabpage(session, a)) 
  }
  def extendedEncode(a: Tabpage): Array[Byte] = {
    a.data 
  }
}
given bufferEncoder: Encoder[Buffer] with {
  def apply(a: Buffer): Value ={
    a.data.asMsgpack
  }
}
given windowEncoder: Encoder[Window] with {
  def apply(a: Window): Value = a.data.asMsgpack
}
given tabpageEncoder: Encoder[Tabpage] with {
  def apply(a: Tabpage): Value = a.data.asMsgpack
}
class Neovim(val in: InputStream, val out: OutputStream)(using ExecutionContext) extends NeovimBase(in, out) {

  object ui {
    def attach(width: Int, height: Int, options: Map[String, Instance[Encoder]]): Unit = {
      session.notify("nvim_ui_attach", width, height, options.map { case (k, v) => (k, v._2(v._1))})
    }
    def detach(): Unit = session.notify("nvim_ui_detach") 
    def tryResize(width: Int, height: Int): Unit = session.notify("nvim_ui_try_resize", width, height)
    object options {
      def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_ui_set_option", name, value)
    }
  }
  def command(command: String): Unit = session.notify("nvim_command", command)
  def feedkeys(keys: String, mode: String, escape_csi: Boolean): Unit =
    session.notify("nvim_feedkeys", keys, mode, escape_csi)
  def input(keys: String): Future[Int] = session.request[Int]("nvim_input", keys)
  def replaceTermcodes(str: String, from_part: Boolean, do_lt: Boolean, special: Boolean): Future[String] =
    session.request[String]("nvim_replace_termcodes", str, from_part, do_lt, special)
  def commandOutput(str: String): Future[String] = session.request[String]("nvim_command_output", str)
  def eval(expr: String): Future[NeovimType] = session.request[NeovimType]("nvim_eval", expr)
  def luaeval(expr: String): Future[NeovimType] = eval(s"""luaeval('$expr')""")
  object funs {
    def apply(name: String)(args: Instance[Encoder]*): Future[NeovimType] = {
      Neovim.this.callFunction(name, args.toArray)
    }
  }
  def callFunction(fname: String, args: Array[Instance[Encoder]]): Future[NeovimType] =
    session.request[NeovimType]("nvim_call_function", fname, args.map(it => it._2(it._1)))
  def strwidth(str: String): Future[Int] = session.request[Int]("nvim_strwidth", str)
  def listRuntimePaths: Future[List[String]] = session.request[List[String]]("nvim_list_runtime_paths")
  def currentDir_=(dir: String): Unit = session.notify("nvim_set_current_dir", dir)
  def getCurrentLine: Future[String] = session.request[String]("nvim_get_current_line")
  def setCurrentLine(line: String): Unit = session.notify("nvim_set_current_line", line)
  def delCurrentLine(): Unit = session.notify("nvim_del_current_line")
  object variables {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_get_var", name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_set_var", name, value)
    def remove(name: String): Unit = session.notify("nvim_del_var", name)
  }
  def getVvar(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_get_vvar", name)
  object options {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_get_option", name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_set_option", name, value)
  }
  object vimout {
    def print(str: String): Unit = session.notify("nvim_out_write", str)
    def println(str: String): Unit = session.notify("nvim_out_write", str + "\n")
  }
  object vimerr {
    def print(str: String): Unit = session.notify("nvim_err_write", str)
    def println(str: String): Unit = session.notify("nvim_err_writeln", str)
  }
  def listBufs: Future[List[Buffer]] = session.request[List[Buffer]](using listCodec(using bufferEncoder)(using bufferCodec(session)))("nvim_list_bufs")
  def currentBuf: Future[Buffer] = session.request[Buffer](using bufferCodec(session))("nvim_get_current_buf")
  def currentBuf_=(buffer: Buffer): Unit = session.notify("nvim_set_current_buf", buffer)
  def listWins: Future[List[Window]] = session.request[List[Window]](using listCodec(using windowEncoder)(using windowCodec(session)))("nvim_list_wins")
  def currentWin: Future[Window] = session.request[Window](using windowCodec(session))("nvim_get_current_win")
  def currentWin_=(window: Window): Unit = session.notify("nvim_set_current_win", window)
  def listTabpages: Future[List[Tabpage]] = session.request[List[Tabpage]](using listCodec(using tabpageEncoder)(using tabpageCodec(session)))("nvim_list_tabpages")
  def currentTabpage: Future[Tabpage] = session.request[Tabpage](using tabpageCodec(session))("nvim_get_current_tabpage")
  def currentTabpage_=(tabpage: Tabpage): Unit = session.notify("nvim_set_current_tabpage", tabpage)
  def subscribe(event: String): Unit = session.notify("nvim_subscribe", event)
  def unsubscribe(event: String): Unit = session.notify("nvim_unsubscribe", event)
  def getColorByName(name: String): Future[Int] = session.request[Int]("nvim_get_color_by_name", name)
  def getColorMap: Future[Map[String, NeovimType]] = session.request[Map[String, NeovimType]]("nvim_get_color_map")
  def getApiInfo: Future[Array[NeovimType]] = session.request[Array[NeovimType]]("nvim_get_api_info")
  def callAtomic(calls: Array[Instance[Encoder]]): Future[Array[NeovimType]] = session.request[Array[NeovimType]]("nvim_call_atomic", calls.map(it => it._2(it._1)))
}
class Buffer(val session: Session, val data: Array[Byte])(using ExecutionContext) extends TypeBase {
  def lineCount: Future[Int] = session.request[Int]("nvim_buf_line_count", this)
  def getLines(start: Int, end: Int, strict_indexing: Boolean): Future[List[String]] =
    session.request[List[String]]("nvim_buf_get_lines", this, start, end, strict_indexing)
  def setLines(start: Int, end: Int, strict_indexing: Boolean, replacement: List[String]): Unit =
    session.notify("nvim_buf_set_lines", this, start, end, strict_indexing, replacement.asMsgpack)
  object variables {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_buf_get_var", Buffer.this, name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_buf_set_var", Buffer.this, name, value)
    def remove(name: String): Unit = session.notify("nvim_buf_del_var", Buffer.this, name)
  }
  object options {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_buf_get_option", Buffer.this, name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_buf_set_option", Buffer.this, name, value)
  }
  def number: Future[Int] = session.request[Int]("nvim_buf_get_number", this)
  def name: Future[String] = session.request[String]("nvim_buf_get_name", this)
  def name_=(name: String): Unit = session.notify("nvim_buf_set_name", this, name)
  def isValid: Future[Boolean] = session.request[Boolean]("nvim_buf_is_valid", this)
  def getMark(name: String): Future[List[Int]] = session.request[List[Int]]("nvim_buf_get_mark", this, name)
  def addHighlight(src_id: Int, hl_group: String, line: Int, col_start: Int, col_end: Int): Future[Int] =
    session.request[Int]("nvim_buf_add_highlight", this, src_id, hl_group, line, col_start, col_end)
  def clearHighlight(src_id: Int, line_start: Int, line_end: Int): Unit =
    session.notify("nvim_buf_clear_highlight", this, src_id, line_start, line_end)
}
class Window(val session: Session, val data: Array[Byte])(using ExecutionContext) extends TypeBase {
  def buffer: Future[Buffer] = session.request[Buffer](using bufferCodec(session))("nvim_win_get_buf", this)
  def cursor: Future[Vec2i] = session.request[Vec2i]("nvim_win_get_cursor", this)
  def setCursor(x: Int, y:Int ): Unit = session.notify("nvim_win_set_cursor", this, List(x, y).asMsgpack)
  def height: Future[Int] = session.request[Int]("nvim_win_get_height", this)
  def height_=(height: Int): Unit = session.notify("nvim_win_set_height", this, height)
  def width: Future[Int] = session.request[Int]("nvim_win_get_width", this)
  def width_=(width: Int): Unit = session.notify("nvim_win_set_width", this, width)
  object variables {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_win_get_var", Window.this, name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_win_set_var", Window.this, name, value)
    def remove(name: String): Unit = session.notify("nvim_win_del_var", Window.this, name)
  }
  object options {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_win_get_option", Window.this, name)
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_win_set_option", Window.this, name, value)
  }
  def position: Future[Vec2i] = session.request[Vec2i]("nvim_win_get_position", this)
  def tabpage: Future[Tabpage] = session.request[Tabpage](using tabpageCodec(session))("nvim_win_get_tabpage", this)
  def number: Future[Int] = session.request[Int]("nvim_win_get_number", this)
  def isValid: Future[Boolean] = session.request[Boolean]("nvim_win_is_valid", this)
}
class Tabpage(val session: Session, val data: Array[Byte])(using ExecutionContext) extends TypeBase {
  def listWins: Future[List[Window]] = session.request[List[Window]](using listCodec(using windowEncoder)(using windowCodec(session)))("nvim_tabpage_list_wins", this) 
  object variables {
    def apply(name: String): Future[NeovimType] = session.request[NeovimType]("nvim_tabpage_get_var", Tabpage.this, name) 
    def update[T](using Encoder[T])(name: String, value: T): Unit = session.notify("nvim_tabpage_set_var", Tabpage.this, name, value)
    def remove(name: String): Unit = session.notify("nvim_tabpage_del_var", Tabpage.this, name)
  }
  def window: Future[Window] = session.request[Window](using windowCodec(session))("nvim_tabpage_get_win", this)
  def number: Future[Int] = session.request[Int]("nvim_tabpage_get_number", this)
  def isValid: Future[Boolean] = session.request[Boolean]("nvim_tabpage_is_valid", this)

}

enum NeovimType {
  case NNil
  case NBoolean(is: Boolean)
  case NInteger(i: Long) 
  case NFloat(d: Double)
  case NString(s: String)
  case NArray(a: Array[NeovimType])
  case NMap(m: Map[String, NeovimType])
  case NObject(t: Byte, bs: Array[Byte])
}
import scala.jdk.CollectionConverters.*
given neovimTypeCodec: Codec[NeovimType] with {
  def apply(a: NeovimType): Value = a match {
    case NeovimType.NNil => ValueFactory.newNil()
    case NeovimType.NArray(a) => a.map(_.asMsgpack).asMsgpack
    case NeovimType.NBoolean(is) => is.asMsgpack
    case NeovimType.NFloat(d) => d.asMsgpack
    case NeovimType.NInteger(i) => i.asMsgpack
    case NeovimType.NString(s) => s.asMsgpack 
    case NeovimType.NMap(m) => m.asMsgpack
    case NeovimType.NObject(t, bs) => ValueFactory.newExtension(t, bs)
  }
  import NeovimType.*
  def apply(v: Value): Option[NeovimType] = {
    if (v.isArrayValue())
      v.as[List[Value]].get.map(_.as[NeovimType]).sequence.map(it => NArray(it.toArray))
    else if (v.isNilValue()) {
      Some(NNil)
    } else if (v.isBooleanValue()){
      Some(NBoolean(v.as[Boolean].get))
    } else if (v.isIntegerValue()) {
      Some(NInteger(v.as[Long].get))
    } else if (v.isFloatValue()) {
      Some(NFloat(v.as[Double].get))
    } else if (v.isStringValue()) {
      Some(NString(v.as[String].get))
    } else if (v.isMapValue()) {
      Some(NMap(v.as[Map[String, Value]].get.map{ case (k, v) => (k, v.as[NeovimType].get)}))
    } else if (v.isExtensionValue()) {
      val ev = v.asExtensionValue()
      Some(NObject(ev.getType(), ev.getData()))
    } else None 
  }
}
