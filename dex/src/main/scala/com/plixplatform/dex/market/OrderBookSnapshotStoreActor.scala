package com.plixplatform.dex.market

import akka.actor.{Actor, Props}
import com.plixplatform.dex.db.OrderBookSnapshotDB
import com.plixplatform.dex.market.OrderBookSnapshotStoreActor._
import com.plixplatform.dex.model.OrderBook.Snapshot
import com.plixplatform.dex.queue.QueueEventWithMeta.Offset
import com.plixplatform.transaction.assets.exchange.AssetPair

class OrderBookSnapshotStoreActor(db: OrderBookSnapshotDB) extends Actor {
  override def receive: Receive = {
    case Message.GetSnapshot(p) => sender() ! Response.GetSnapshot(db.get(p))

    case Message.Update(p, offset, newSnapshot) =>
      db.update(p, offset, newSnapshot)
      sender() ! Response.Updated(offset)

    case Message.Delete(p) => db.delete(p)
  }
}

object OrderBookSnapshotStoreActor {
  sealed trait Message
  object Message {
    case class GetSnapshot(assetPair: AssetPair) extends Message

    /**
      * @param newSnapshot None if it wasn't changed
      */
    case class Update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[Snapshot]) extends Message

    case class Delete(assetPair: AssetPair) extends Message
  }

  sealed trait Response
  object Response {
    case class GetSnapshot(result: Option[(Offset, Snapshot)]) extends Response
    case class Updated(offset: Offset)                         extends Response
    case class Deleted(assetPair: AssetPair)                   extends Response
  }

  def props(db: OrderBookSnapshotDB): Props = Props(new OrderBookSnapshotStoreActor(db))
}
