package net.bulbyvr.neovim

import net.bulbyvr.msgpack.Session

abstract class TypeBase {
  val session: Session
  val data: Array[Byte]

  override def equals(o: Any) = o match {
    case that: TypeBase => this.data.sameElements(that.data)
    case _ => false
  }
}
