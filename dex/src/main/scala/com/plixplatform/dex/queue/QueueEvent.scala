package com.plixplatform.dex.queue

import com.plixplatform.common.state.ByteStr
import com.plixplatform.crypto.DigestSize
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order}

sealed trait QueueEvent extends Product with Serializable {
  def assetPair: AssetPair
}

object QueueEvent {
  case class Placed(order: Order) extends QueueEvent {
    override def assetPair: AssetPair = order.assetPair
  }
  case class Canceled(assetPair: AssetPair, orderId: Order.Id) extends QueueEvent
  case class OrderBookDeleted(assetPair: AssetPair)            extends QueueEvent

  def toBytes(x: QueueEvent): Array[Byte] = x match {
    case Placed(order)                => (1: Byte) +: order.version +: order.bytes()
    case Canceled(assetPair, orderId) => (2: Byte) +: (assetPair.bytes ++ orderId.arr)
    case OrderBookDeleted(assetPair)  => (3: Byte) +: assetPair.bytes
  }

  def fromBytes(xs: Array[Byte]): QueueEvent = xs.head match {
    case 1 => QueueEvent.Placed(Order.fromBytes(xs(1), xs.slice(2, Int.MaxValue)))
    case 2 =>
      val assetPair = AssetPair.fromBytes(xs.tail)
      QueueEvent.Canceled(assetPair, ByteStr(xs.takeRight(DigestSize)))
    case 3 => OrderBookDeleted(AssetPair.fromBytes(xs.tail))
    case x => throw new IllegalArgumentException(s"Unknown event type: $x")
  }
}
