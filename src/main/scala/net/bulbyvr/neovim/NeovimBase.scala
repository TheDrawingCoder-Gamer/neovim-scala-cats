package net.bulbyvr.neovim

import java.io.{InputStream, OutputStream}

import net.bulbyvr.msgpack.{ResponseHandler, Session}
import org.msgpack.value.Value
import net.bulbyvr.msgpack.*

class NeovimBase(in: InputStream, out: OutputStream) {
  val session = Session(in, out)
  def onRequest(callback: (String, List[NeovimType], ResponseHandler) => Unit): Unit = session.onRequest((s, v, r) => callback(s, v.map(_.as[NeovimType].get), r))
  def onNotification(callback: (String, List[NeovimType]) => Unit): Unit = session.onNotification((s, v) => callback(s, v.map(_.as[NeovimType].get)))
}
